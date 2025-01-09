{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Compiler.Backend where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (nub, sort)

import Parser.Abs
import Aux
import State
import qualified Control.Monad
import Control.Monad.State
import GHC.TopHandler (runIO)
import System.Process (system)


runCompiler :: Program -> String -> IO RegisterAndType
runCompiler program fileNameWithPath = runIO $ evalStateT (generateLLVMProgram program fileNameWithPath) initialState


generateLLVMProgram :: Program -> String -> CompilerM RegisterAndType
generateLLVMProgram (Program _ topDefs) fileNameWithPath = do
  -- extract all function signatures and add them to states
  topDefFunIdents <- mapM (\(FnDef _ _ ident _ _) -> return ident) topDefs
  insertIdentFunSigs $ zip topDefFunIdents (map fromFnDefToFunValue topDefs)

  let builtInFunctionsFiltered = filter (\(IFunDecl _ ident _) -> ident `notElem` topDefFunIdents) builtInFunctions
  let funValsFiltered = map fromFunDeclToFunValue builtInFunctionsFiltered
  mapM_ addGenLLVM builtInFunctionsFiltered
  builtInFunctionIdents <- mapM (\(IFunDecl _ ident _) -> return ident) builtInFunctionsFiltered
  insertIdentFunSigs $ zip builtInFunctionIdents funValsFiltered

  topDefsLLVM <- mapM generateLLVMTopDef topDefs
  genLLVM <- getGenLLVM
  -- get all strings and put them on the top
  globalStrings <- gets globalStringMap
  let globalStringsLLVM = map (\(num, str) -> IStringGlobal (Ident $ show num) str) $ Map.toList globalStrings
  let genLLVMWithStrings = globalStringsLLVM ++ genLLVM
  let outputFilePath = reverse (drop 4 (reverse fileNameWithPath)) ++ ".ll"
  let outputBCFilePath = reverse (drop 4 (reverse fileNameWithPath)) ++ ".bc"
  liftIO $ do
    writeFile outputFilePath (unlines (map show genLLVMWithStrings))
    _ <- system $ "llvm-as " ++ outputFilePath ++ " -o " ++ outputBCFilePath
    _ <- system $ "llvm-link " ++ outputFilePath ++ " lib/runtime.bc -o " ++ outputBCFilePath
    return ()
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
  setDoNotReturn False
  return dummyReturnRegisterAndType


generateLLVMBlock :: Block -> CompilerM (Map Ident RegisterAndType)
generateLLVMBlock (Block _ stmts) = go stmts ([], Map.empty)
  where
    go :: [Stmt] -> ([Ident], Map Ident RegisterAndType) -> CompilerM (Map Ident RegisterAndType)
    go [] (_, changed) = return changed
    go (stmt : rest) (currDecl, changed) = do
      (newDecl, newChanged) <- generateLLVMStmt stmt (currDecl, changed)
      go rest (newDecl, newChanged)


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
  newStringNum <- getNextStringNumAndIncrement
  insertStringToGlobalStringMap newStringNum s -- will be put on the top
  newStringReg <- getNextRegisterAndIncrement
  addGenLLVM $ IAss (EVReg newStringReg) (EVString s newStringNum)
  return (newStringReg, TVString)

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
  case exprRegType1 of
    TVInt -> do
      addGenLLVM $ IBinOp (EVReg resultReg) (EVReg exprReg1) (EVReg exprReg2) binOp
      return (resultReg, exprRegType1)
    TVString -> do
      addGenLLVM $ IFunCall (EVReg resultReg) TVString (Ident "_strcat") [(TVString, EVReg exprReg1), (TVString, EVReg exprReg2)]
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
  case exprRegType1 of
    TVInt -> do
      addGenLLVM $ IRelOp (EVReg resultReg) TVInt (EVReg exprReg1) (EVReg exprReg2) relOp
      return (resultReg, TVBool)
    TVString -> do
      addGenLLVM $ IFunCall (EVReg resultReg) TVBool (Ident "_strcmp") [(TVString, EVReg exprReg1), (TVString, EVReg exprReg2)]
      return (resultReg, TVBool)
    TVBool -> do
      addGenLLVM $ IRelOp (EVReg resultReg) TVBool (EVReg exprReg1) (EVReg exprReg2) relOp
      return (resultReg, TVBool)

generateLLVMExpr (EAnd _ expr1 expr2) = generateLazyAndOr expr1 expr2 BAnd

generateLLVMExpr (EOr _ expr1 expr2) = generateLazyAndOr expr1 expr2 BOr

generateLLVMExpr (EApp _ ident exprs) = do
  identToFunSig <- gets identToFunSig

  oldVars <- gets identToRegisterAndType

  let funSig = identToFunSig Map.! ident
  regAndTypes <- mapM generateLLVMExpr exprs

  let (EVFun retType _ argTypeRegs block) = funSig

  modify $ \s -> s { identToRegisterAndType = oldVars }
  regForFunRes <- if retType == TVVoid
                  then return (-1) -- dummy value for void return type
                  else getNextRegisterAndIncrement
  if retType == TVVoid
    then addGenLLVM $ IFunCallVoid ident (map (\(reg, typ) -> (typ, EVReg reg)) regAndTypes)
    else addGenLLVM $ IFunCall (EVReg regForFunRes) retType ident (map (\(reg, typ) -> (typ, EVReg reg)) regAndTypes)
  return (regForFunRes, retType)


generateLazyAndOr :: Expr -> Expr -> DBinOp -> CompilerM RegisterAndType
generateLazyAndOr expr1 expr2 op = do
  (reg1, _) <- generateLLVMExpr expr1

  currLabel <- getCurrentBasicBlockLabel

  trueReg <- getNextRegisterAndIncrement
  addGenLLVM $ IAss (EVReg trueReg) (EVBool True)

  falseReg <- getNextRegisterAndIncrement
  addGenLLVM $ IAss (EVReg falseReg) (EVBool False)

  labelRight <- getNextLabelAndIncrement
  labelEnd   <- getNextLabelAndIncrement
  resultReg  <- getNextRegisterAndIncrement

  case op of
    BAnd -> addGenLLVM $ IBr (EVReg reg1) labelRight labelEnd
    BOr  -> addGenLLVM $ IBr (EVReg reg1) labelEnd   labelRight

  insertEmptyBasicBlock labelRight
  setcurrBasicBlockLabel labelRight
  addGenLLVM $ ILabel labelRight

  (reg2, _) <- generateLLVMExpr expr2
  labelRightEnd <- getCurrentBasicBlockLabel
  addGenLLVM $ IBrJump labelEnd

  insertEmptyBasicBlock labelEnd
  setcurrBasicBlockLabel labelEnd
  addGenLLVM $ ILabel labelEnd

  let shortCircuitVal = case op of
        BAnd -> EVReg falseReg
        BOr  -> EVReg trueReg

  addGenLLVM $ IPhi (EVReg resultReg) TVBool
                    (shortCircuitVal, currLabel)
                    (EVReg reg2, labelRightEnd)

  return (resultReg, TVBool)


generateIncDec :: Ident -> DBinOp -> CompilerM (Register, LLVMType)
generateIncDec ident op = do
    (identReg, identRegType) <- lookupIdentRegisterAndType ident
    oneConstReg <- getNextRegisterAndIncrement
    addGenLLVM $ IAss (EVReg oneConstReg) (EVInt 1)
    resultReg <- getNextRegisterAndIncrement
    addGenLLVM $ IBinOp (EVReg resultReg) (EVReg identReg) (EVReg oneConstReg) op
    insertIdentRegisterAndType ident resultReg identRegType
    return (resultReg, identRegType)


generateLLVMStmt :: Stmt -> ([Ident], Map Ident RegisterAndType) -> CompilerM ([Ident], Map Ident RegisterAndType)
generateLLVMStmt (Ass _ ident expr) (currDecl, prevDeclAndChanged) = do
  (exprReg, exprRegType) <- generateLLVMExpr expr
  insertIdentRegisterAndType ident exprReg exprRegType
  (currDecl', prevDeclAndChanged') <- if ident `elem` currDecl
                      then return (currDecl, prevDeclAndChanged)
                      else return (currDecl, Map.insert ident (exprReg, exprRegType) prevDeclAndChanged)
  return (currDecl', prevDeclAndChanged')

generateLLVMStmt (Decl _ itemsType items) (currDecl, prevDeclAndChanged) = do
  (currDecl', prevDeclAndChanged') <- foldM
    (\(currDeclAcc, prevDeclAndChangedAcc) declItem -> case declItem of
      NoInit _ ident -> do
        reg <- getNextRegisterAndIncrement
        let llvmItemsType = bnfcTypeToLLVMType itemsType
        addGenLLVM $ IAss (EVReg reg) (getValueDefaultInit llvmItemsType)
        insertIdentRegisterAndType ident reg llvmItemsType
        return (currDeclAcc ++ [ident], prevDeclAndChangedAcc)
      Init _ ident expr -> do
        identToReg <- gets identToRegisterAndType
        (_, _) <- generateLLVMStmt (Ass BNFC'NoPosition ident expr) (currDeclAcc, prevDeclAndChangedAcc)
        return (currDeclAcc ++ [ident], prevDeclAndChangedAcc)
    )
    (currDecl, prevDeclAndChanged)
    items
  return (currDecl', prevDeclAndChanged')

generateLLVMStmt (Ret _ expr) (currDecl, prevDeclAndChanged) = do
  shouldNotReturn <- getDoNotReturn
  (exprReg, exprRegType) <- generateLLVMExpr expr
  Control.Monad.when shouldNotReturn $ do
    setRetValue (exprReg, exprRegType)
  addGenLLVM $ IFunRet (EVReg exprReg) exprRegType
  setRetValue (exprReg, exprRegType)
  return (currDecl, prevDeclAndChanged)

generateLLVMStmt (VRet _) (currDecl, prevDeclAndChanged) = do
  voidReg <- getNextRegisterAndIncrement
  addGenLLVM $ IFunRet (EVReg voidReg) TVVoid
  setRetValue (-1, TVVoid)
  return (currDecl, prevDeclAndChanged)

generateLLVMStmt (Empty _) (currDecl, prevDeclAndChanged) = return (currDecl, prevDeclAndChanged)

generateLLVMStmt (BStmt _ block) (currDecl, prevDeclAndChanged) = do
  oldMap <- gets identToRegisterAndType
  newMap <- generateLLVMBlock block
  let mergedMap = Map.union newMap oldMap
  let updatedResult = Map.union newMap prevDeclAndChanged
  modify $ \st -> st { identToRegisterAndType = mergedMap }
  return (currDecl, updatedResult)

generateLLVMStmt (Incr _ ident) (currDecl, prevDeclAndChanged) = do
  (reg, regType) <- generateIncDec ident BAdd
  if ident `elem` currDecl
    then return (currDecl, prevDeclAndChanged)
    else return (currDecl, Map.insert ident (reg, regType) prevDeclAndChanged)

generateLLVMStmt (Decr _ ident) (currDecl, prevDeclAndChanged) = do
  (reg, regType) <- generateIncDec ident BSub
  if ident `elem` currDecl
    then return (currDecl, prevDeclAndChanged)
    else return (currDecl, Map.insert ident (reg, regType) prevDeclAndChanged)

generateLLVMStmt (SExp _ expr) (currDecl, prevDeclAndChanged) = do
  (_, _) <- generateLLVMExpr expr
  return (currDecl, prevDeclAndChanged)

generateLLVMStmt (Cond _ expr stmt) (currDecl, prevDeclAndChanged) = generateLLVMStmt (CondElse BNFC'NoPosition expr stmt (Empty BNFC'NoPosition)) (currDecl, prevDeclAndChanged)

generateLLVMStmt (CondElse _ expr stmt1 stmt2) (currDecl, prevDeclAndChanged) = do
  (exprReg, _) <- generateLLVMExpr expr
  if isELitTrue expr
    then do
      (_, _) <- generateLLVMStmt stmt1 ([], Map.empty)
      return (currDecl, prevDeclAndChanged)
    else do
      if isELitFalse expr
        then do
          (_, _) <- generateLLVMStmt stmt2 ([], Map.empty)
          return (currDecl, prevDeclAndChanged)
        else do
          currState <- get
          doNotReturnOld <- getDoNotReturn
          setDoNotReturn True

          labelTrue <- getNextLabelAndIncrement
          labelFalse <- getNextLabelAndIncrement
          labelEnd  <- getNextLabelAndIncrement

          -- generate conditional jump and get current label and state
          addGenLLVM $ IBr (EVReg exprReg) labelTrue labelFalse
          currLabel <- getCurrentBasicBlockLabel
          oldState <- get
          let oldIdentToRegisterAndType = identToRegisterAndType oldState

          -- generate code for true branch
          insertEmptyBasicBlock labelTrue
          setcurrBasicBlockLabel labelTrue
          addGenLLVM $ ILabel labelTrue
          (_, prevDeclAndChanged1) <- generateLLVMStmt stmt1 ([], Map.empty)
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
          (_, prevDeclAndChanged2) <- generateLLVMStmt stmt2 ([], Map.empty)
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

          -- PHI for variables changed in both branches
          let phiInstrs = nub (sort (Map.keys (Map.intersection prevDeclAndChanged1 prevDeclAndChanged2)))
          let phiInstrsWithTypes = 
                map (\ident -> (ident, (prevDeclAndChanged1 Map.! ident, prevDeclAndChanged2 Map.! ident)))
                    phiInstrs
          phiRes <- forM phiInstrsWithTypes $ \(ident, (reg1, reg2)) -> do
            let (r1, t1) = reg1
            let (r2, t2) = reg2
            r1' <- getNextRegisterAndIncrement
            addGenLLVM $ IPhi (EVReg r1') t1 (EVReg r1, currentBl1) (EVReg r2, currentBl2)
            insertIdentRegisterAndType ident r1' t1
            return (ident, (r1', t1))

          -- PHI for variables changed in only one branch
          let phiInstrs1 = nub (sort (Map.keys (Map.difference prevDeclAndChanged1 prevDeclAndChanged2)))
          let phiInstrsWithTypes1 = map (\ident -> (ident, prevDeclAndChanged1 Map.! ident)) phiInstrs1
          phiRes1 <- forM phiInstrsWithTypes1 $ \(ident, (reg, t)) -> do
            r' <- getNextRegisterAndIncrement
            (oldRegister, _) <- lookupIdentRegisterAndType ident
            addGenLLVM $ IPhi (EVReg r') t (EVReg oldRegister, currentBl2) (EVReg reg, currentBl1)
            insertIdentRegisterAndType ident r' t
            return (ident, (r', t))

          let phiInstrs2 = nub (sort (Map.keys (Map.difference prevDeclAndChanged2 prevDeclAndChanged1)))
          let phiInstrsWithTypes2 = map (\ident -> (ident, prevDeclAndChanged2 Map.! ident)) phiInstrs2
          phiRes2 <- forM phiInstrsWithTypes2 $ \(ident, (reg, t)) -> do
            r' <- getNextRegisterAndIncrement
            (oldRegister, _) <- lookupIdentRegisterAndType ident
            addGenLLVM $ IPhi (EVReg r') t (EVReg oldRegister, currentBl1) (EVReg reg, currentBl2)
            insertIdentRegisterAndType ident r' t
            return (ident, (r', t))

          setDoNotReturn doNotReturnOld

          -- add return if return occured in both branches
          Control.Monad.when (retInTrue && retInFalse) $ do
              setRetValueToDummy
              setDoNotReturn False
              retNewReg <- getNextRegisterAndIncrement
              let returnType = snd retValueInTrue
              case returnType of
                TVVoid -> do
                  addGenLLVM $ IFunRet EVVoid TVVoid
                  setRetValue (-1, TVVoid)
                  return ()
                _ -> do
                  newReg <- getNextRegisterAndIncrement
                  addGenLLVM $ IAss (EVReg newReg) (getValueDefaultInit returnType)
                  addGenLLVM $ IFunRet (EVReg newReg) returnType
                  setRetValue (newReg, returnType)
            
          let phiMap = Map.fromList $ phiRes ++ phiRes1 ++ phiRes2
          return (currDecl, Map.union phiMap prevDeclAndChanged)

generateLLVMStmt (While _ expr stmt) (currDecl, prevDeclAndChanged) = do
  oldState <- get
  oldRegisterAndTypeMap <- gets identToRegisterAndType

  currLabel <- getCurrentBasicBlockLabel
  labelBefore <- getNextLabelAndIncrement
  labelCond <- getNextLabelAndIncrement
  labelBody <- getNextLabelAndIncrement
  labelEnd <- getNextLabelAndIncrement

  insertEmptyBasicBlock labelBody
  setcurrBasicBlockLabel labelBody
  (currDecl', prevDeclAndChanged') <- generateLLVMStmt stmt ([], Map.empty)
  endingBodyLabel <- getCurrentBasicBlockLabel

  -- generate phi nodes for variables that were changed in the loop
  insertEmptyBasicBlock labelBefore
  setcurrBasicBlockLabel labelBefore
  addGenLLVM $ IBrJump labelBefore
  addGenLLVM $ ILabel labelBefore
  let phiInstrs = nub (sort (Map.keys prevDeclAndChanged'))
  let phiInstrsWithTypes = map (\ident -> (ident, prevDeclAndChanged' Map.! ident)) phiInstrs
  phiRes <- forM phiInstrsWithTypes $ \(ident, (reg, t)) -> do
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

  -- insert labelBefore and labelCond blocks again along wiht their instructions
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

  forM_ phiRes $ \(ident, (reg, t)) -> do
    insertIdentRegisterAndType ident reg t

  (_, _) <- generateLLVMStmt stmt ([], Map.empty)
  addGenLLVM $ IBr (EVReg expr) labelBefore labelEnd

  forM_ phiRes $ \(ident, (reg, t)) -> do
    insertIdentRegisterAndType ident reg t

  insertEmptyBasicBlock labelEnd
  setcurrBasicBlockLabel labelEnd
  addGenLLVM $ ILabel labelEnd

  modify $ \s -> s { nextFreeRegNum = expr + 1 }

  return (currDecl, Map.union (Map.fromList phiRes) prevDeclAndChanged)


