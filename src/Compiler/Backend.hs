{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Compiler.Backend where

import Data.Map (Map)
import qualified Data.Map as Map

import Parser.Abs
import Utils.Types
import Utils.Aux
import Utils.State
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import GHC.TopHandler (runIO)

import qualified Data.List.Unique as LU
import qualified Data.List as L
import Debug.Trace
import Data.Map.Internal.Debug (node)
import qualified Control.Monad


-- TODO: 
-- while DONE
-- if in while DONE
-- decl WITH INIT in block DONE
-- while with very simple body DONE
-- decrement and increment DONE
-- function calls basic DONE
-- function calls recursion DONE
-- print DONE partially
-- preprend function declarations DONE 
-- returns in if (and also void returns) DONE
-- lazy evaluation
-- read
-- string manipulation


runCompiler :: Program -> IO RegisterAndType
runCompiler program = runIO $ evalStateT (generateLLVMProgram program) initialState


generateLLVMProgram :: Program -> CompilerM RegisterAndType
generateLLVMProgram (Program _ topDefs) = do
  -- extract all function signatures and add them to states - potentially dangerous
  topDefFunIdents <- mapM (\(FnDef _ _ ident _ _) -> return ident) topDefs
  insertIdentFunSigs $ zip topDefFunIdents (map fromFnDefToFunValue topDefs)

  let builtInFunctionsFiltered = filter (\(IFunDecl _ ident _) -> ident `notElem` topDefFunIdents) builtInFunctions
  let funValsFiltered = map fromFunDeclToFunValue builtInFunctionsFiltered
  mapM_ addGenLLVM builtInFunctionsFiltered
  builtInFunctionIdents <- mapM (\(IFunDecl _ ident _) -> return ident) builtInFunctionsFiltered
  insertIdentFunSigs $ zip builtInFunctionIdents funValsFiltered

  topDefsLLVM <- mapM generateLLVMTopDef topDefs
  genLLVM <- getGenLLVM
  liftIO $ writeFile "output.ll" (unlines (map show genLLVM))
  return dummyReturnRegisterAndType


generateLLVMTopDef :: TopDef -> CompilerM RegisterAndType
generateLLVMTopDef (FnDef _ retType ident args block) = do
  let argTypes = map (\(Arg _ t _) -> t) args
  -- reserve registers for arguments
  argTypeRegPairs <- foldM (\acc (Arg _ t ident) -> do
    reg <- getNextRegisterAndIncrement
    insertIdentRegisterAndType ident reg (bnfcTypeToLLVMType t)
    return $ acc ++ [(bnfcTypeToLLVMType t, EVReg reg)]
    ) [] args

  let funValue = EVFun (bnfcTypeToLLVMType retType) ident argTypeRegPairs block
  modify $ \s -> s { identToFunSig = Map.insert ident funValue (identToFunSig s) }
  label <- getNextLabelAndIncrement -- get new label for function
  modify $ \s -> s { currBasicBlockLabel = label }
  insertEmptyBasicBlock label
  addGenLLVM $ IFunPr (bnfcTypeToLLVMType retType) ident argTypeRegPairs
  addGenLLVM $ ILabel label
  _ <- generateLLVMBlock block
  Control.Monad.when (bnfcTypeToLLVMType retType == TVVoid && not (doesBlockContainVRet block)) $ do
    addGenLLVM $ IFunRet EVVoid TVVoid
  addGenLLVM IFunEp
  state <- get
  put $ setIdentToRegisterAndTypeToEmpty state
  return dummyReturnRegisterAndType


generateLLVMBlock :: Block -> CompilerM (Map Ident RegisterAndType) -- TODO: Reduce
generateLLVMBlock (Block _ stmts) = do
  let comp = \(inner, outer) -> \s -> generateLLVMStmt s (inner, outer)
  (innerRes, res) <- foldM comp ([], Map.empty) stmts
  return res


generateLLVMExpr :: Expr -> CompilerM RegisterAndType
generateLLVMExpr (EVar _ ident) = do
  identToReg <- gets identToRegisterAndType
  case Map.lookup ident identToReg of
    Just (reg, typ) -> return (reg, typ)
    Nothing -> error $ "Variable " ++ extractIdent ident ++ " not found"

generateLLVMExpr (ELitInt _ i) = do
  reg <- getNextRegisterAndIncrement
  addGenLLVM $ IAss (EVReg reg) (EVInt $ fromIntegral i)
  return (reg, TVInt)

generateLLVMExpr (ELitTrue _) = do
  reg <- getNextRegisterAndIncrement
  addGenLLVM $ IAss (EVReg reg) (EVBool True)
  return (reg, TVBool)

generateLLVMExpr (ELitFalse _) = do
  reg <- getNextRegisterAndIncrement
  addGenLLVM $ IAss (EVReg reg) (EVBool False)
  return (reg, TVBool)

generateLLVMExpr (EString _ s) = do
  reg <- getNextRegisterAndIncrement
  addGenLLVM $ IAss (EVReg reg) (EVString s)
  return (reg, TVString)

generateLLVMExpr (Neg _ expr) = do
  (exprReg, exprRegType) <- generateLLVMExpr expr
  -- generate negative by subtracting from 0
  zeroReg <- getNextRegisterAndIncrement
  addGenLLVM $ IAss (EVReg zeroReg) (EVInt 0)
  resReg <- getNextRegisterAndIncrement
  addGenLLVM $ IBinOp (EVReg resReg) (EVReg zeroReg) (EVReg exprReg) BSub
  return (resReg, exprRegType)

generateLLVMExpr (Not _ expr) = do
  (exprReg, exprRegType) <- generateLLVMExpr expr
  trueReg <- getNextRegisterAndIncrement
  addGenLLVM $ IAss (EVReg trueReg) (EVBool True)
  resReg <- getNextRegisterAndIncrement
  addGenLLVM $ IRelOp (EVReg resReg) exprRegType (EVReg exprReg) (EVReg trueReg) RE
  return (resReg, TVBool)

generateLLVMExpr (EMul _ expr1 mulOp expr2) = do
  (exprReg1, exprRegType1) <- generateLLVMExpr expr1
  (exprReg2, exprRegType2) <- generateLLVMExpr expr2
  resultReg <- getNextRegisterAndIncrement
  let binOp = case mulOp of
        Times _ -> BMul
        Div _ -> BDiv
        Mod _ -> BMod
  addGenLLVM $ IBinOp (EVReg resultReg) (EVReg exprReg1) (EVReg exprReg2) binOp
  return (resultReg, exprRegType1)

generateLLVMExpr (EAdd _ expr1 addOp expr2) = do
  (exprReg1, exprRegType1) <- generateLLVMExpr expr1
  (exprReg2, exprRegType2) <- generateLLVMExpr expr2
  resultReg <- getNextRegisterAndIncrement
  let binOp = case addOp of
        Plus _ -> BAdd
        Minus _ -> BSub
  addGenLLVM $ IBinOp (EVReg resultReg) (EVReg exprReg1) (EVReg exprReg2) binOp
  return (resultReg, exprRegType1)

generateLLVMExpr (ERel _ expr1 oper expr2) = do
  (exprReg1, exprRegType1) <- generateLLVMExpr expr1
  (exprReg2, exprRegType2) <- generateLLVMExpr expr2
  resultReg <- getNextRegisterAndIncrement
  let relOp = case oper of
        LTH _ -> RLTH
        LE _ -> RLE
        GTH _ -> RGTH
        GE _ -> RGE
        EQU _ -> RQU
        NE _ -> RE
  addGenLLVM $ IRelOp (EVReg resultReg) exprRegType1 (EVReg exprReg1) (EVReg exprReg2) relOp
  return (resultReg, TVBool)

generateLLVMExpr (EAnd _ expr1 expr2) = generateLazyAndOr expr1 expr2 BAnd

generateLLVMExpr (EOr _ expr1 expr2) = generateLazyAndOr expr1 expr2 BOr

generateLLVMExpr (EApp _ ident exprs) = do
  identToFunSig <- gets identToFunSig
  -- oldState <- get
  -- oldGenInstrFromCurrBlock <- getGenLLVM
  -- oldRegNum <- gets nextFreeRegNum
  -- oldLabelNum <- gets nextFreeLabelNum

  oldVars <- gets identToRegisterAndType

  -- What to do:
  -- 1. Preserve the old state and generate expressions that will be args
  -- 2. Get function argument types and var names
  -- 3. Get a brand new state (almost empty, but with functions)
  -- 4. Write to this state args with values 

  let funSig = identToFunSig Map.! ident
  regAndTypes <- mapM generateLLVMExpr exprs

  -- newGenInstrFromCurrBlock <- getGenLLVM
  -- let generatedInstrs = newGenInstrFromCurrBlock L.\\ oldGenInstrFromCurrBlock
  -- newRegNum <- gets nextFreeRegNum
  -- newLabelNum <- gets nextFreeLabelNum

  let (EVFun retType _ argTypeRegs block) = funSig

  -- call the function
  -- put oldState
  modify $ \s -> s { identToRegisterAndType = oldVars }
  -- let howMuchToIncrementReg = newRegNum - oldRegNum
  -- let howMuchToIncrementLabel = newLabelNum - oldLabelNum
  -- increment register counter by the number of registers used in generating exprs
  -- modify $ \s -> s { nextFreeRegNum = oldRegNum + howMuchToIncrementReg }
  -- increment label counter by the number of labels used in generating exprs
  -- liftIO $ putStrLn $ "howMuchToIncrementLabel: " ++ show howMuchToIncrementLabel
  -- modify $ \s -> s { nextFreeLabelNum = oldLabelNum + howMuchToIncrementLabel }
  -- modify $ \s -> s { currBasicBlockLabel = oldLabelNum + howMuchToIncrementLabel - 1 }
  regForFunRes <- if retType == TVVoid
                  then return (-1) -- dummy value for void return type
                  else getNextRegisterAndIncrement
  -- mapM_ addGenLLVM generatedInstrs
  if retType == TVVoid
    then addGenLLVM $ IFunCallVoid ident (map (\(reg, typ) -> (typ, EVReg reg)) regAndTypes)
    else addGenLLVM $ IFunCall (EVReg regForFunRes) retType ident (map (\(reg, typ) -> (typ, EVReg reg)) regAndTypes)
  return (regForFunRes, retType)
  

generateLazyAndOr :: Expr -> Expr -> DBinOp -> CompilerM RegisterAndType
generateLazyAndOr expr1 expr2 op = do
  (reg1, typ1) <- generateLLVMExpr expr1

  currLabel <- getCurrentBasicBlockLabel
  liftIO $ putStrLn $ "Current label: " ++ show currLabel

  -- Create a register for 'true' and 'false' constants (sometimes reused).
  trueReg <- getNextRegisterAndIncrement
  addGenLLVM $ IAss (EVReg trueReg) (EVBool True)
  falseReg <- getNextRegisterAndIncrement
  addGenLLVM $ IAss (EVReg falseReg) (EVBool False)

  -- We'll need three labels:
  --   labelRight:   evaluate expr2
  --   labelEnd:     merge results
  labelRight <- getNextLabelAndIncrement
  labelEnd   <- getNextLabelAndIncrement
  -- For the final result
  resultReg <- getNextRegisterAndIncrement

   -- 1. Short-circuit branching
  --    For AND: if expr1 == false => skip expr2, result = false
  --    For OR:  if expr1 == true  => skip expr2, result = true
  let jumpIf = case op of
        BAnd -> (EVReg reg1, labelRight, labelEnd)  -- if reg1==true => labelRight else => labelEnd
        BOr  -> (EVReg reg1, labelEnd,   labelRight) -- if reg1==true => labelEnd   else => labelRight

  addGenLLVM $ IBr (fst3 jumpIf) (snd3 jumpIf) (thd3 jumpIf)

  -- 2. labelRight: evaluate expr2
  insertEmptyBasicBlock labelRight
  setcurrBasicBlockLabel labelRight
  addGenLLVM $ ILabel labelRight

  (reg2, _) <- generateLLVMExpr expr2
  labelRightEnd <- getCurrentBasicBlockLabel
  addGenLLVM $ IBrJump labelEnd

  -- 3. labelEnd: create phi node
  insertEmptyBasicBlock labelEnd
  setcurrBasicBlockLabel labelEnd
  addGenLLVM $ ILabel labelEnd

  -- In the phi:
  -- For AND:
  --   - coming from labelRight => the value is reg2
  --   - coming from "short-circuit" => the value is false
  --
  -- For OR:
  --   - coming from labelRight => the value is reg2
  --   - coming from short-circuit => the value is true
  --
  let (shortCircuitLabel, shortCircuitVal) = case op of
        BAnd -> (thd3 jumpIf, EVReg falseReg)
        BOr  -> (thd3 jumpIf, EVReg trueReg)

  addGenLLVM $ IPhi (EVReg resultReg) TVBool
    (shortCircuitVal, currLabel) -- short-circuit path
    (EVReg reg2, labelRightEnd)          -- normal path

  -- Return the final register (Boolean)
  return (resultReg, TVBool)

  where
    fst3 (x,_,_) = x
    snd3 (_,y,_) = y
    thd3 (_,_,z) = z


generateIncDec :: Ident -> DBinOp -> CompilerM (Register, LLVMType)
generateIncDec ident op = do
    (identReg, identRegType) <- lookupIdentRegisterAndType ident
    oneConstReg <- getNextRegisterAndIncrement
    addGenLLVM $ IAss (EVReg oneConstReg) (EVInt 1)
    resultReg <- getNextRegisterAndIncrement
    addGenLLVM $ IBinOp (EVReg resultReg) (EVReg identReg) (EVReg oneConstReg) op
    insertIdentRegisterAndType ident resultReg identRegType
    return (resultReg, identRegType)


-- inner - list of variables already declared in the current block
-- outer - map of variables that were declared in the outer block and now changed in the current block
-- TODO: change inner and outer names
generateLLVMStmt :: Stmt -> ([Ident], Map Ident RegisterAndType) -> CompilerM ([Ident], Map Ident RegisterAndType)
generateLLVMStmt (Ass _ ident expr) (inner, outer) = do
  (exprReg, exprRegType) <- generateLLVMExpr expr   -- generate code for expression and store result in register
  insertIdentRegisterAndType ident exprReg exprRegType
  (inner', outer') <- if ident `elem` inner
                      then return (inner, outer)
                      else return (inner, Map.insert ident (exprReg, exprRegType) outer)
  return (inner', outer')

generateLLVMStmt (Decl _ itemsType items) (inner, outer) = do
  (inner', outer') <- foldM
    (\(innerAcc, outerAcc) declItem -> case declItem of
      NoInit _ ident -> do
        reg <- getNextRegisterAndIncrement
        let llvmItemsType = bnfcTypeToLLVMType itemsType
        addGenLLVM $ IAss (EVReg reg) (getValueDefaultInit llvmItemsType)
        insertIdentRegisterAndType ident reg llvmItemsType
        return (innerAcc ++ [ident], outerAcc)
      Init _ ident expr -> do
        identToReg <- gets identToRegisterAndType
        (_, _) <- generateLLVMStmt (Ass BNFC'NoPosition ident expr) (innerAcc, outerAcc)
        return (innerAcc ++ [ident], outerAcc)
    )
    (inner, outer)
    items
  return (inner', outer')

generateLLVMStmt (Ret _ expr) (inner, outer) = do
  shouldNotReturn <- getDoNotReturn
  if shouldNotReturn then do
    (exprReg, exprRegType) <- generateLLVMExpr expr
    setRetValue (exprReg, exprRegType)
    return (inner, outer)
    else do
      (exprReg, exprRegType) <- generateLLVMExpr expr
      addGenLLVM $ IFunRet (EVReg exprReg) exprRegType
      setRetValue (exprReg, exprRegType)
      return (inner, outer)

generateLLVMStmt (VRet _) (inner, outer) = do
  shouldNotReturn <- getDoNotReturn
  if shouldNotReturn then do
    voidReg <- getNextRegisterAndIncrement
    setRetValue (-1, TVVoid)
    return (inner, outer)
    else do
      voidReg <- getNextRegisterAndIncrement
      addGenLLVM $ IFunRet (EVReg voidReg) TVVoid
      setRetValue (-1, TVVoid)
      return (inner, outer)

generateLLVMStmt (Empty _) (inner, outer) = return (inner, outer)

generateLLVMStmt (BStmt _ block) (inner, outer) = do
  currIdentToRegisterAndType <- gets identToRegisterAndType
  out <- generateLLVMBlock block
  modify $ \s -> s { identToRegisterAndType = Map.union out currIdentToRegisterAndType }
  return (inner, Map.union out outer)

generateLLVMStmt (Incr _ ident) (inner, outer) = do
  (reg, regType) <- generateIncDec ident BAdd
  if ident `elem` inner
    then return (inner, outer)
    else return (inner, Map.insert ident (reg, regType) outer)

generateLLVMStmt (Decr _ ident) (inner, outer) = do -- TODO: needs changing outer
  -- we need to change outer if the variable was declared in the outer block
  (reg, regType) <- generateIncDec ident BSub
  if ident `elem` inner
    then return (inner, outer)
    else return (inner, Map.insert ident (reg, regType) outer)

generateLLVMStmt (SExp _ expr) (inner, outer) = do
  (_, _) <- generateLLVMExpr expr
  return (inner, outer)

generateLLVMStmt (Cond _ expr stmt) (inner, outer) = generateLLVMStmt (CondElse BNFC'NoPosition expr stmt (Empty BNFC'NoPosition)) (inner, outer)

generateLLVMStmt (CondElse _ expr stmt1 stmt2) (inner, outer) = do
  (exprReg, _) <- generateLLVMExpr expr
  -- check if the expr is ELitTrue or ELitFalse
  -- check if if condition was literal true or false
  if isELitTrue expr
    then do
      (_, _) <- generateLLVMStmt stmt1 ([], Map.empty)
      return (inner, outer)
    else do
      if isELitFalse expr
        then do
          (_, _) <- generateLLVMStmt stmt2 ([], Map.empty)
          return (inner, outer)
        else do
          currState <- get
          doNotReturnOld <- getDoNotReturn
          setDoNotReturn True

          labelTrue <- getNextLabelAndIncrement
          labelFalse <- getNextLabelAndIncrement
          labelEnd  <- getNextLabelAndIncrement

          -- Generate conditional jump and get current label and state
          addGenLLVM $ IBr (EVReg exprReg) labelTrue labelFalse
          currLabel <- getCurrentBasicBlockLabel
          oldState <- get
          let oldIdentToRegisterAndType = identToRegisterAndType oldState

          -- generate code for true branch
          insertEmptyBasicBlock labelTrue
          setcurrBasicBlockLabel labelTrue
          addGenLLVM $ ILabel labelTrue
          (_, outer1) <- generateLLVMStmt stmt1 ([], Map.empty)
          currentBl1 <- getCurrentBasicBlockLabel
          addGenLLVM $ IBrJump labelEnd
          mm1 <- gets identToRegisterAndType
          modify $ \s -> s { identToRegisterAndType = oldIdentToRegisterAndType }
          retInTrue <- checkIfReturnValueNotDummy
          retValueInTrue <- getRetValue
          setRetValueToDummy


          -- generate code for false branch
          insertEmptyBasicBlock labelFalse
          setcurrBasicBlockLabel labelFalse
          addGenLLVM $ ILabel labelFalse
          (_, outer2) <- generateLLVMStmt stmt2 ([], Map.empty)
          currentBl2 <- getCurrentBasicBlockLabel
          addGenLLVM $ IBrJump labelEnd
          mm2 <- gets identToRegisterAndType
          modify $ \s -> s { identToRegisterAndType = oldIdentToRegisterAndType }
          retInFalse <- checkIfReturnValueNotDummy
          retValueInFalse <- getRetValue
          setRetValueToDummy

          insertEmptyBasicBlock labelEnd
          setcurrBasicBlockLabel labelEnd
          addGenLLVM $ ILabel labelEnd

          -- generate phi statements for variables that were changed in both branches
          let phiNodes = LU.sortUniq $ Map.keys $ Map.intersection outer1 outer2
          let phiNodesWithTypes = map (\ident -> (ident, (outer1 Map.! ident, outer2 Map.! ident))) phiNodes
          phiRes <- forM phiNodesWithTypes $ \(ident, (reg1, reg2)) -> do
            let (r1, t1) = reg1
            let (r2, t2) = reg2
            r1' <- getNextRegisterAndIncrement
            addGenLLVM $ IPhi (EVReg r1') t1 (EVReg r1, currentBl1) (EVReg r2, currentBl2)
            insertIdentRegisterAndType ident r1' t1
            return (ident, (r1', t1))

          -- generate phi statements for variables that were changed in one branch
          let phiNodes1 = LU.sortUniq $ Map.keys $ Map.difference outer1 outer2
          let phiNodesWithTypes1 = map (\ident -> (ident, outer1 Map.! ident)) phiNodes1
          phiRes1 <- forM phiNodesWithTypes1 $ \(ident, (reg, t)) -> do
            r' <- getNextRegisterAndIncrement
            (oldRegister, _) <- lookupIdentRegisterAndType ident
            addGenLLVM $ IPhi (EVReg r') t (EVReg oldRegister, currentBl2) (EVReg reg, currentBl1)
            insertIdentRegisterAndType ident r' t
            return (ident, (r', t))

          -- generate phi statements for variables that were changed in one branch
          let phiNodes2 = LU.sortUniq $ Map.keys $ Map.difference outer2 outer1
          let phiNodesWithTypes2 = map (\ident -> (ident, outer2 Map.! ident)) phiNodes2
          phiRes2 <- forM phiNodesWithTypes2 $ \(ident, (reg, t)) -> do
            r' <- getNextRegisterAndIncrement
            (oldRegister, _) <- lookupIdentRegisterAndType ident
            addGenLLVM $ IPhi (EVReg r') t (EVReg oldRegister, currentBl1) (EVReg reg, currentBl2)
            insertIdentRegisterAndType ident r' t
            return (ident, (r', t))

          setDoNotReturn doNotReturnOld

          -- add return if return occured in both branches
          Control.Monad.when (retInTrue && retInFalse) $ do
              setRetValueToDummy
              retNewReg <- getNextRegisterAndIncrement
              addGenLLVM $ IPhi (EVReg retNewReg) (snd retValueInTrue) (EVReg $ fst retValueInTrue, currentBl1) (EVReg $ fst retValueInFalse, currentBl2)
              setRetValue (retNewReg, snd retValueInTrue)
              unless doNotReturnOld $ do addRetValueGenLLVM

          -- accumplate outers
          let phiMap = Map.fromList $ phiRes ++ phiRes1 ++ phiRes2
          return (inner, Map.union phiMap outer)

-- TODO: Return flag does not work in while
generateLLVMStmt (While _ expr stmt) (inner, outer) = do
  oldState <- get
  oldRegisterAndTypeMap <- gets identToRegisterAndType

  currLabel <- getCurrentBasicBlockLabel
  labelBefore <- getNextLabelAndIncrement
  labelCond <- getNextLabelAndIncrement
  labelBody <- getNextLabelAndIncrement
  labelEnd <- getNextLabelAndIncrement

  insertEmptyBasicBlock labelBody -- might not be the best idea but works for now
  setcurrBasicBlockLabel labelBody
  (inner', outer') <- generateLLVMStmt stmt ([], Map.empty)
  endingBodyLabel <- getCurrentBasicBlockLabel

  -- generate phi nodes for variables that were changed in the loop
  insertEmptyBasicBlock labelBefore
  setcurrBasicBlockLabel labelBefore
  addGenLLVM $ IBrJump labelBefore
  addGenLLVM $ ILabel labelBefore
  let phiNodes = LU.sortUniq $ Map.keys outer'
  let phiNodesWithTypes = map (\ident -> (ident, outer' Map.! ident)) phiNodes
  phiRes <- forM phiNodesWithTypes $ \(ident, (reg, t)) -> do
    r' <- getNextRegisterAndIncrement
    let (oldRegister, _) = oldRegisterAndTypeMap Map.! ident
    addGenLLVM $ IPhi (EVReg r') t (EVReg oldRegister, currLabel) (EVReg reg, endingBodyLabel)
    insertIdentRegisterAndType ident r' t
    return (ident, (r', t))

  insertEmptyBasicBlock labelCond
  setcurrBasicBlockLabel labelCond
  addGenLLVM $ IBrJump labelCond
  addGenLLVM $ ILabel labelCond
  (expr, _) <- generateLLVMExpr expr

  addGenLLVM $ IBr (EVReg expr) labelBody labelEnd

  instrBeforeAcc <- getBasicBlockGenLLVM labelBefore -- get generated phi instructions
  instrCondAcc <- getBasicBlockGenLLVM labelCond -- get cond generated LLVM

  put oldState -- restore state before the loop
  modify $ \s -> s { nextFreeLabelNum = labelEnd + 1 }
  -- increment label count by 2 because of the before and cond labels that already exist

  -- insert labelBefore and labelCond blocks again along with their instructions
  insertEmptyBasicBlock labelBefore
  setcurrBasicBlockLabel labelBefore
  mapM_ addGenLLVM instrBeforeAcc

  insertEmptyBasicBlock labelCond
  setcurrBasicBlockLabel labelCond
  mapM_ addGenLLVM instrCondAcc

  instrBef <- getBasicBlockGenLLVM labelBefore
  instrC <- getBasicBlockGenLLVM labelCond

  insertEmptyBasicBlock labelBody
  setcurrBasicBlockLabel labelBody
  addGenLLVM $ ILabel labelBody

  -- insert state variables by phi
  forM_ phiRes $ \(ident, (reg, t)) -> do
    insertIdentRegisterAndType ident reg t

  (_, _) <- generateLLVMStmt stmt ([], Map.empty)
  addGenLLVM $ IBr (EVReg expr) labelBefore labelEnd

  -- insert state variables by phi - not sure
  forM_ phiRes $ \(ident, (reg, t)) -> do
    insertIdentRegisterAndType ident reg t

  insertEmptyBasicBlock labelEnd
  setcurrBasicBlockLabel labelEnd
  addGenLLVM $ ILabel labelEnd


  -- set register counter to expr + 1
  modify $ \s -> s { nextFreeRegNum = expr + 1 }

  return (inner, Map.union (Map.fromList phiRes) outer)


