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


-- TODO: 
-- while DONE
-- if in while DONE
-- decl WITH INIT in block DONE
-- while with very simple body DONE
-- decrement and increment DONE
-- function calls basic DONE
-- function calls recursion DONE
-- print
-- preprend function declarations
-- returns in if
-- read
-- string manipulation
-- lazy evaluation


runCompiler :: Program -> IO RegisterAndType
runCompiler program = runIO $ evalStateT (generateLLVMProgram program) initialState


generateLLVMProgram :: Program -> CompilerM RegisterAndType
generateLLVMProgram (Program _ topDefs) = do
  mapM_ addGenLLVM builtInFunctions
  let funVals = map fromFunDeclToFunValue builtInFunctions
  builtInFunctionIdents <- mapM (\(IFunDecl _ ident _) -> return ident) builtInFunctions
  -- extract all function signatures and add their declarations to the beginning of the program and add them to state
  insertIdentFunSigs $ zip builtInFunctionIdents funVals

  -- extract all function signatures and add them to state
  topDefFunIdents <- mapM (\(FnDef _ _ ident _ _) -> return ident) topDefs
  insertIdentFunSigs $ zip topDefFunIdents (map fromFnDefToFunValue topDefs)
  -- generate declarations
  -- mapM_ (addGenLLVM . fromLLVMValueToInstr . fromFnDefToFunValue) topDefs

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
  addGenLLVM IFunEp
  state <- get
  put $ setIdentToRegisterAndTypeToEmpty state
  return dummyReturnRegisterAndType


generateLLVMBlock :: Block -> CompilerM (Map Ident RegisterAndType) -- TODO: Reduce
generateLLVMBlock (Block _ stmts) = do
  let comp = \(inner, outer) -> \s -> generateLLVMStmt s (inner, outer)
  (_, res) <- foldM comp ([], Map.empty) stmts
  liftIO $ print $ "res: " ++ show res
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
  resultReg <- getNextRegisterAndIncrement
  addGenLLVM $ IAss (EVReg resultReg) (EVReg exprReg)
  return (resultReg, exprRegType)

generateLLVMExpr (Not _ expr) = do
  (exprReg, exprRegType) <- generateLLVMExpr expr
  resultReg <- getNextRegisterAndIncrement
  addGenLLVM $ IAss (EVReg resultReg) (EVReg exprReg)
  return (resultReg, exprRegType)

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
  addGenLLVM $ IRelOp (EVReg resultReg) (EVReg exprReg1) (EVReg exprReg2) relOp
  return (resultReg, TVBool)

generateLLVMExpr (EAnd _ expr1 expr2) = do
  (exprReg1, exprRegType1) <- generateLLVMExpr expr1
  (exprReg2, exprRegType2) <- generateLLVMExpr expr2
  resultReg <- getNextRegisterAndIncrement
  addGenLLVM $ IBinOp (EVReg resultReg) (EVReg exprReg1) (EVReg exprReg2) BAnd
  return (resultReg, TVBool)

generateLLVMExpr (EOr _ expr1 expr2) = do
  (exprReg1, exprRegType1) <- generateLLVMExpr expr1
  (exprReg2, exprRegType2) <- generateLLVMExpr expr2
  resultReg <- getNextRegisterAndIncrement
  addGenLLVM $ IBinOp (EVReg resultReg) (EVReg exprReg1) (EVReg exprReg2) BOr
  return (resultReg, TVBool)

generateLLVMExpr (EApp _ ident exprs) = do
  identToFunSig <- gets identToFunSig
  oldState <- get
  oldGenInstrFromCurrBlock <- getBasicBlockGenLLVM (currBasicBlockLabel oldState)
  oldRegNum <- gets nextFreeRegNum

  -- What to do:
  -- 1. Preserve the old state and generate expressions that will be args
  -- 2. Get function argument types and var names
  -- 3. Get a brand new state (almost empty, but with functions)
  -- 4. Write to this state args with values 

  let funSig = identToFunSig Map.! ident
  regAndTypes <- mapM generateLLVMExpr exprs

  newGenInstrFromCurrBlock <- getBasicBlockGenLLVM (currBasicBlockLabel oldState)
  let generatedInstrs = newGenInstrFromCurrBlock L.\\ oldGenInstrFromCurrBlock
  newRegNum <- gets nextFreeRegNum

  let (EVFun retType _ argTypeRegs block) = funSig

  -- call the function
  put oldState
  let howMuchToIncrement = newRegNum - oldRegNum
  -- increment register counter by the number of registers used in generating exprs
  modify $ \s -> s { nextFreeRegNum = oldRegNum + howMuchToIncrement }
  regForFunRes <- if retType == TVVoid
                  then return (-1) -- dummy value for void return type
                  else getNextRegisterAndIncrement
  mapM_ addGenLLVM generatedInstrs
  if retType == TVVoid
    then addGenLLVM $ IFunCallVoid ident (map (\(reg, typ) -> (typ, EVReg reg)) regAndTypes)
    else addGenLLVM $ IFunCall (EVReg regForFunRes) retType ident (map (\(reg, typ) -> (typ, EVReg reg)) regAndTypes)
  return (regForFunRes, retType)



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
        if ident `Map.member` identToReg
          then do
            reg <- getNextRegisterAndIncrement
            let llvmItemsType = bnfcTypeToLLVMType itemsType
            addGenLLVM $ IAss (EVReg reg) (getValueDefaultInit llvmItemsType)
            insertIdentRegisterAndType ident reg llvmItemsType
            return (innerAcc ++ [ident], outerAcc)
          else do
            (_, _) <- generateLLVMStmt (Ass BNFC'NoPosition ident expr) (innerAcc, outerAcc)
            return (innerAcc ++ [ident], outerAcc)
    )
    (inner, outer)
    items
  return (inner', outer')

generateLLVMStmt (Ret _ expr) (inner, outer) = do
  (exprReg, exprRegType) <- generateLLVMExpr expr
  addGenLLVM $ IFunRet (EVReg exprReg) exprRegType
  return (inner, outer)

generateLLVMStmt (VRet _) (inner, outer) = do
  voidReg <- getNextRegisterAndIncrement
  addGenLLVM $ IFunRet (EVReg voidReg) TVVoid
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
  currState <- get

  liftIO $ print ("currentLabel: " ++ show (nextFreeLabelNum currState))
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

  -- generate code for false branch
  insertEmptyBasicBlock labelFalse
  setcurrBasicBlockLabel labelFalse
  addGenLLVM $ ILabel labelFalse
  (_, outer2) <- generateLLVMStmt stmt2 ([], Map.empty)
  currentBl2 <- getCurrentBasicBlockLabel
  addGenLLVM $ IBrJump labelEnd
  mm2 <- gets identToRegisterAndType
  modify $ \s -> s { identToRegisterAndType = oldIdentToRegisterAndType }

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

  -- accumplate outers
  let phiMap = Map.fromList $ phiRes ++ phiRes1 ++ phiRes2
  return (inner, Map.union phiMap outer)

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
  liftIO $ print $ "----outer----: " ++ show outer
  -- print current next register number
  liftIO $ print $ "----nextFreeRegNum----: " ++ show (nextFreeRegNum oldState)

  -- generate phi nodes for variables that were changed in the loop
  insertEmptyBasicBlock labelBefore
  setcurrBasicBlockLabel labelBefore
  addGenLLVM $ IBrJump labelBefore
  addGenLLVM $ ILabel labelBefore
  let phiNodes = LU.sortUniq $ Map.keys outer'
  liftIO $ print $ "----phiNodes----: " ++ show phiNodes
  let phiNodesWithTypes = map (\ident -> (ident, outer' Map.! ident)) phiNodes
  phiRes <- forM phiNodesWithTypes $ \(ident, (reg, t)) -> do
    r' <- getNextRegisterAndIncrement
    liftIO $ print $ "----r'----: " ++ show r'
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

  liftIO $ print $ "----instrBeforeAcc----: " ++ show instrBeforeAcc
  liftIO $ print $ "----instrCondAcc----: " ++ show instrCondAcc

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
  liftIO $ print $ "----instrBef----: " ++ show instrBef
  instrC <- getBasicBlockGenLLVM labelCond
  liftIO $ print $ "----instrC----: " ++ show instrC

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

  liftIO $ print $ "----phiRes----: " ++ show phiRes
  --print current register number
  liftIO $ print $ "----nextFreeRegNum----: " ++ show (nextFreeRegNum oldState)

  -- set register counter to expr + 1
  modify $ \s -> s { nextFreeRegNum = expr + 1 }

  return (inner, Map.union (Map.fromList phiRes) outer)
  

