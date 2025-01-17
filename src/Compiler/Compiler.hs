{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Compiler.Compiler where

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


runCompiler :: Program -> String -> IO ValueAndType
runCompiler program fileNameWithPath = runIO $ evalStateT (generateLLVMProgram program fileNameWithPath) initialState


generateLLVMProgram :: Program -> String -> CompilerM ValueAndType
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
  return dummyReturnValueAndType


generateLLVMTopDef :: TopDef -> CompilerM ValueAndType
generateLLVMTopDef (FnDef _ retType ident args block) = do
  let argTypes = map (\(Arg _ t _) -> t) args
  -- reserve registers for arguments
  argTypeRegPairs <- foldM (\acc (Arg _ t ident) -> do
    reg <- getNextRegisterAndIncrement
    insertIdentValueAndType ident (EVReg reg) (bnfcTypeToLLVMType t)
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
  put $ setIdentToValueAndTypeToEmpty state
  setDoNotReturn False
  return dummyReturnValueAndType


generateLLVMBlock :: Block -> CompilerM (Map Ident ValueAndType)
generateLLVMBlock (Block _ stmts) = go stmts ([], Map.empty)
  where
    go :: [Stmt] -> ([Ident], Map Ident ValueAndType) -> CompilerM (Map Ident ValueAndType)
    go [] (_, changed) = return changed
    go (stmt : rest) (currDecl, changed) = do
      (newDecl, newChanged) <- generateLLVMStmt stmt (currDecl, changed)
      go rest (newDecl, newChanged)


generateLLVMExpr :: Expr -> CompilerM ValueAndType
generateLLVMExpr (EVar _ ident) = do
  identToReg <- gets identToValueAndType
  case Map.lookup ident identToReg of
    Just (reg, typ) -> return (reg, typ)
    Nothing -> error $ "Variable " ++ extractIdent ident ++ " not found"

generateLLVMExpr (ELitInt _ i) = do
  return (EVInt $ fromIntegral i, TVInt)

generateLLVMExpr (ELitTrue _) = do
  return (EVBool True, TVBool)

generateLLVMExpr (ELitFalse _) = do
  return (EVBool False, TVBool)

generateLLVMExpr (EString _ s) = do
  newStringNum <- getNextStringNumAndIncrement
  insertStringToGlobalStringMap newStringNum s -- will be put on the top
  newStringReg <- getNextRegisterAndIncrement
  addGenLLVM $ IAss (EVReg newStringReg) (EVString s newStringNum)
  return (EVReg newStringReg, TVString)

generateLLVMExpr (Neg _ expr) = do
  (exprReg, exprRegType) <- generateLLVMExpr expr
  -- generate negative by subtracting from 0
  zeroReg <- getNextRegisterAndIncrement
  addGenLLVM $ IAss (EVReg zeroReg) (EVInt 0)
  resReg <- getNextRegisterAndIncrement
  addGenLLVM $ IBinOp (EVReg resReg) (EVReg zeroReg) exprReg BSub
  return (EVReg resReg, exprRegType)

generateLLVMExpr (Not _ expr) = do
  (exprReg, exprRegType) <- generateLLVMExpr expr
  trueReg <- getNextRegisterAndIncrement
  addGenLLVM $ IAss (EVReg trueReg) (EVBool True)
  resReg <- getNextRegisterAndIncrement
  addGenLLVM $ IRelOp (EVReg resReg) exprRegType exprReg (EVReg trueReg) RE
  return (EVReg resReg, TVBool)

generateLLVMExpr (EMul _ expr1 mulOp expr2) = do
  (exprReg1, exprRegType1) <- generateLLVMExpr expr1
  (exprReg2, exprRegType2) <- generateLLVMExpr expr2
  resultReg <- getNextRegisterAndIncrement
  let binOp = case mulOp of
        Times _ -> BMul
        Div _ -> BDiv
        Mod _ -> BMod
  addGenLLVM $ IBinOp (EVReg resultReg) exprReg1 exprReg2 binOp
  return (EVReg resultReg, exprRegType1)

generateLLVMExpr (EAdd _ expr1 addOp expr2) = do
  (exprReg1, exprRegType1) <- generateLLVMExpr expr1
  (exprReg2, exprRegType2) <- generateLLVMExpr expr2
  resultReg <- getNextRegisterAndIncrement
  let binOp = case addOp of
        Plus _ -> BAdd
        Minus _ -> BSub
  case exprRegType1 of
    TVInt -> do
      addGenLLVM $ IBinOp (EVReg resultReg) exprReg1 exprReg2 binOp
      return (EVReg resultReg, exprRegType1)
    TVString -> do
      addGenLLVM $ IFunCall (EVReg resultReg) TVString (Ident "_strcat") [(TVString, exprReg1), (TVString, exprReg2)]
      return (EVReg resultReg, exprRegType1)

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
      addGenLLVM $ IRelOp (EVReg resultReg) TVInt exprReg1 exprReg2 relOp
      return (EVReg resultReg, TVBool)
    TVString -> do
      addGenLLVM $ IFunCall (EVReg resultReg) TVBool (Ident "_strcmp") [(TVString, exprReg1), (TVString, exprReg2)]
      return (EVReg resultReg, TVBool)
    TVBool -> do
      addGenLLVM $ IRelOp (EVReg resultReg) TVBool exprReg1 exprReg2 relOp
      return (EVReg resultReg, TVBool)

generateLLVMExpr (EAnd _ expr1 expr2) = generateLazyAndOr expr1 expr2 BAnd

generateLLVMExpr (EOr _ expr1 expr2) = generateLazyAndOr expr1 expr2 BOr

generateLLVMExpr (EApp _ ident exprs) = do
  identToFunSig <- gets identToFunSig

  oldVars <- gets identToValueAndType

  let funSig = identToFunSig Map.! ident
  regAndTypes <- mapM generateLLVMExpr exprs

  let (EVFun retType _ argTypeRegs block) = funSig

  modify $ \s -> s { identToValueAndType = oldVars }
  regForFunRes <- if retType == TVVoid
                  then return (-1) -- dummy value for void return type
                  else getNextRegisterAndIncrement
  if retType == TVVoid
    then addGenLLVM $ IFunCallVoid ident (map (\(reg, typ) -> (typ, reg)) regAndTypes)
    else addGenLLVM $ IFunCall (EVReg regForFunRes) retType ident (map (\(reg, typ) -> (typ, reg)) regAndTypes)
  return (EVReg regForFunRes, retType)


generateLazyAndOr :: Expr -> Expr -> DBinOp -> CompilerM ValueAndType
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
    BAnd -> addGenLLVM $ IBr reg1 labelRight labelEnd
    BOr  -> addGenLLVM $ IBr reg1 labelEnd   labelRight

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
                    (reg2, labelRightEnd)

  return (EVReg resultReg, TVBool)


generateIncDec :: Ident -> DBinOp -> CompilerM (Register, LLVMType)
generateIncDec ident op = do
    (identReg, identRegType) <- lookupIdentValueAndType ident
    oneConstReg <- getNextRegisterAndIncrement
    addGenLLVM $ IAss (EVReg oneConstReg) (EVInt 1)
    resultReg <- getNextRegisterAndIncrement
    addGenLLVM $ IBinOp (EVReg resultReg) identReg (EVReg oneConstReg) op
    insertIdentValueAndType ident (EVReg resultReg) identRegType
    return (resultReg, identRegType)


generateLLVMStmt :: Stmt -> ([Ident], Map Ident ValueAndType) -> CompilerM ([Ident], Map Ident ValueAndType)
generateLLVMStmt (Ass _ ident expr) (currDecl, prevDeclAndChanged) = do
  (exprReg, exprRegType) <- generateLLVMExpr expr
  insertIdentValueAndType ident exprReg exprRegType
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
        insertIdentValueAndType ident (EVReg reg) llvmItemsType
        return (currDeclAcc ++ [ident], prevDeclAndChangedAcc)
      Init _ ident expr -> do
        identToReg <- gets identToValueAndType
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
  addGenLLVM $ IFunRet exprReg exprRegType
  setRetValue (exprReg, exprRegType)
  return (currDecl, prevDeclAndChanged)

generateLLVMStmt (VRet _) (currDecl, prevDeclAndChanged) = do
  voidReg <- getNextRegisterAndIncrement
  addGenLLVM $ IFunRet (EVReg voidReg) TVVoid
  setRetValue (EVReg (-1), TVVoid)
  return (currDecl, prevDeclAndChanged)

generateLLVMStmt (Empty _) (currDecl, prevDeclAndChanged) = return (currDecl, prevDeclAndChanged)

generateLLVMStmt (BStmt _ block) (currDecl, prevDeclAndChanged) = do
  oldMap <- gets identToValueAndType
  newMap <- generateLLVMBlock block
  let mergedMap = Map.union newMap oldMap
  let updatedResult = Map.union newMap prevDeclAndChanged
  modify $ \st -> st { identToValueAndType = mergedMap }
  return (currDecl, updatedResult)

generateLLVMStmt (Incr _ ident) (currDecl, prevDeclAndChanged) = do
  (reg, regType) <- generateIncDec ident BAdd
  if ident `elem` currDecl
    then return (currDecl, prevDeclAndChanged)
    else return (currDecl, Map.insert ident (EVReg reg, regType) prevDeclAndChanged)

generateLLVMStmt (Decr _ ident) (currDecl, prevDeclAndChanged) = do
  (reg, regType) <- generateIncDec ident BSub
  if ident `elem` currDecl
    then return (currDecl, prevDeclAndChanged)
    else return (currDecl, Map.insert ident (EVReg reg, regType) prevDeclAndChanged)

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
          addGenLLVM $ IBr exprReg labelTrue labelFalse
          currLabel <- getCurrentBasicBlockLabel
          oldState <- get
          let oldIdentToValueAndType = identToValueAndType oldState

          -- generate code for true branch
          insertEmptyBasicBlock labelTrue
          setcurrBasicBlockLabel labelTrue
          addGenLLVM $ ILabel labelTrue
          (_, prevDeclAndChanged1) <- generateLLVMStmt stmt1 ([], Map.empty)
          currentBl1 <- getCurrentBasicBlockLabel
          addGenLLVM $ IBrJump labelEnd
          modify $ \s -> s { identToValueAndType = oldIdentToValueAndType }
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
          modify $ \s -> s { identToValueAndType = oldIdentToValueAndType }
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
            addGenLLVM $ IPhi (EVReg r1') t1 (r1, currentBl1) (r2, currentBl2)
            insertIdentValueAndType ident (EVReg r1') t1
            return (ident, (EVReg r1', t1))

          -- PHI for variables changed in only one branch
          let phiInstrs1 = nub (sort (Map.keys (Map.difference prevDeclAndChanged1 prevDeclAndChanged2)))
          let phiInstrsWithTypes1 = map (\ident -> (ident, prevDeclAndChanged1 Map.! ident)) phiInstrs1
          phiRes1 <- forM phiInstrsWithTypes1 $ \(ident, (reg, t)) -> do
            r' <- getNextRegisterAndIncrement
            (oldRegister, _) <- lookupIdentValueAndType ident
            addGenLLVM $ IPhi (EVReg r') t (oldRegister, currentBl2) (reg, currentBl1)
            insertIdentValueAndType ident (EVReg r') t
            return (ident, (EVReg r', t))

          let phiInstrs2 = nub (sort (Map.keys (Map.difference prevDeclAndChanged2 prevDeclAndChanged1)))
          let phiInstrsWithTypes2 = map (\ident -> (ident, prevDeclAndChanged2 Map.! ident)) phiInstrs2
          phiRes2 <- forM phiInstrsWithTypes2 $ \(ident, (reg, t)) -> do
            r' <- getNextRegisterAndIncrement
            (oldRegister, _) <- lookupIdentValueAndType ident
            addGenLLVM $ IPhi (EVReg r') t (oldRegister, currentBl1) (reg, currentBl2)
            insertIdentValueAndType ident (EVReg r') t
            return (ident, (EVReg r', t))

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
                  setRetValue (EVReg (-1), TVVoid)
                  return ()
                _ -> do
                  newReg <- getNextRegisterAndIncrement
                  addGenLLVM $ IAss (EVReg newReg) (getValueDefaultInit returnType)
                  addGenLLVM $ IFunRet (EVReg newReg) returnType
                  setRetValue (EVReg newReg, returnType)

          let phiMap = Map.fromList $ phiRes ++ phiRes1 ++ phiRes2
          return (currDecl, Map.union phiMap prevDeclAndChanged)

generateLLVMStmt (While _ expr stmt) (currDecl, prevDeclAndChanged) = do
  oldState <- get
  oldValueAndTypeMap <- gets identToValueAndType

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
    let (oldRegister, _) = oldValueAndTypeMap Map.! ident
    addGenLLVM $ IPhi (EVReg r') t (oldRegister, currLabel) (reg, endingBodyLabel)
    insertIdentValueAndType ident (EVReg r') t
    return (ident, (EVReg r', t))

  insertEmptyBasicBlock labelCond
  setcurrBasicBlockLabel labelCond
  addGenLLVM $ IBrJump labelCond
  addGenLLVM $ ILabel labelCond
  (expr, _) <- generateLLVMExpr expr

  addGenLLVM $ IBr expr labelBody labelEnd

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
    insertIdentValueAndType ident reg t

  (_, _) <- generateLLVMStmt stmt ([], Map.empty)
  addGenLLVM $ IBr expr labelBefore labelEnd

  forM_ phiRes $ \(ident, (reg, t)) -> do
    insertIdentValueAndType ident reg t

  insertEmptyBasicBlock labelEnd
  setcurrBasicBlockLabel labelEnd
  addGenLLVM $ ILabel labelEnd

  modify $ \s -> s { nextFreeRegNum = extractRegisterValue expr + 1 }

  return (currDecl, Map.union (Map.fromList phiRes) prevDeclAndChanged)


