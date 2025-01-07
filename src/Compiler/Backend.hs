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
import Data.List as L
import Debug.Trace
import Data.Map.Internal.Debug (node)



-- TODO: 
-- phi (variable overshadowing)
-- function calls
-- if, if else, while
-- block
-- read, print


runCompiler :: Program -> IO RegisterAndType
runCompiler program = runIO $ evalStateT (generateLLVMProgram program) initialState


generateLLVMProgram :: Program -> CompilerM RegisterAndType
generateLLVMProgram (Program _ topDefs) = do
  topDefsLLVM <- mapM generateLLVMTopDef topDefs
  genLLVM <- getGenLLVM
  liftIO $ writeFile "output.ll" (unlines (map show genLLVM))
  return dummyReturnRegisterAndType


generateLLVMTopDef :: TopDef -> CompilerM RegisterAndType
generateLLVMTopDef (FnDef _ retType ident args block) = do
  let argTypes = map (\(Arg _ t _) -> t) args
  let funValue = EVFun (bnfcTypeToLLVMType retType) ident args block
  modify $ \s -> s { identToFunSig = Map.insert ident funValue (identToFunSig s) }
  label <- getNextLabelAndIncrement -- get new label for function
  modify $ \s -> s { currBasicBlockLabel = label }
  insertEmptyBasicBlock label
  addGenLLVM $ IFunPr (bnfcTypeToLLVMType retType) ident (map bnfcTypeToLLVMType argTypes)
  addGenLLVM $ ILabel label
  _ <- generateLLVMBlock block
  addGenLLVM IFunEp
  return dummyReturnRegisterAndType

-- TODO: Reduce
generateLLVMBlock :: Block -> CompilerM (Map Ident RegisterAndType)
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

  let funSig = identToFunSig Map.! ident
  regAndTypes <- mapM generateLLVMExpr exprs
  let (EVFun retType _ args block) = funSig
  let argTypes = map (\(Arg _ t _) -> t) args
  let argNames = map (\(Arg _ _ ident) -> ident) args
  let argNamesWithTypes = zip argNames argTypes
  let argNamesWithTypes' = zip argNames regAndTypes

  return dummyReturnRegisterAndType
-- add new env

generateIncDec :: Ident -> DBinOp -> CompilerM (Register, LLVMType)
generateIncDec ident op = do
    (identReg, identRegType) <- lookupIdentRegisterAndType ident
    newReg <- getNextRegisterAndIncrement
    addGenLLVM $ IBinOp (EVReg newReg) (EVReg identReg) (EVInt 1) op
    insertIdentRegisterAndType ident newReg identRegType
    return (newReg, identRegType)


-- inner - list of variables already declared in the current block
-- outer - map of variables that were declared in the outer block and now changed in the current block
generateLLVMStmt :: Stmt -> ([Ident], Map Ident RegisterAndType) -> CompilerM ([Ident], Map Ident RegisterAndType)
generateLLVMStmt (Ass _ ident expr) (inner, outer) = do
  _ <- trace ("ASsing inner: " ++ show inner ++ " ASsing outer: " ++ show outer) $ return ([], Map.empty)
  (exprReg, exprRegType) <- generateLLVMExpr expr   -- generate code for expression and store result in register
  insertIdentRegisterAndType ident exprReg exprRegType
  (inner', outer') <- if ident `elem` inner 
                      then return (inner, outer) 
                      else return (inner, Map.insert ident (exprReg, exprRegType) outer)
  _ <- trace ("ASsing inner': " ++ show inner' ++ " ASsing outer': " ++ show outer') $ return ([], Map.empty)
  return (inner', outer')

generateLLVMStmt (Decl _ itemsType items) (inner, outer) = do
  _ <- trace ("starting inner: " ++ show inner ++ "starting outer: " ++ show outer) $ return ([], Map.empty)
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
            (innerAcc', outerAcc') <- generateLLVMStmt (Ass BNFC'NoPosition ident expr) (innerAcc, outerAcc)
            return (innerAcc' ++ [ident], outerAcc')
    )
    (inner, outer)
    items
  _ <- trace ("starting inner': " ++ show inner' ++ "starting outer': " ++ show outer') $ return ([], Map.empty)
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
  generateIncDec ident BAdd
  return (inner, outer)

generateLLVMStmt (Decr _ ident) (inner, outer) = do -- TODO: needs changing outer
  generateIncDec ident BSub
  return (inner, outer)

generateLLVMStmt (SExp _ expr) (inner, outer) = do
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
  -- let oldIdentToFunSig = identToFunSig oldState 

  -- generate code for true branch
  addGenLLVM $ ILabel labelTrue
  setcurrBasicBlockLabel labelTrue
  insertEmptyBasicBlock labelTrue
  (_, outer1) <- generateLLVMStmt stmt1 ([], Map.empty)
  currentBl1 <- getCurrentBasicBlockLabel
  addGenLLVM $ IBrJump labelEnd
  mm1 <- gets identToRegisterAndType
  modify $ \s -> s { identToRegisterAndType = oldIdentToRegisterAndType }

  -- generate code for false branch
  addGenLLVM $ ILabel labelFalse
  setcurrBasicBlockLabel labelFalse
  insertEmptyBasicBlock labelFalse
  (_, outer2) <- generateLLVMStmt stmt2 ([], Map.empty)
  currentBl2 <- getCurrentBasicBlockLabel
  addGenLLVM $ IBrJump labelEnd
  mm2 <- gets identToRegisterAndType
  modify $ \s -> s { identToRegisterAndType = oldIdentToRegisterAndType }

  addGenLLVM $ ILabel labelEnd
  insertEmptyBasicBlock labelEnd

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

  -- trace label true
  liftIO $ print $ "labelTrue: " ++ show labelTrue
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
  setcurrBasicBlockLabel labelEnd
  return (inner, Map.union phiMap outer)

generateLLVMStmt (While _ expr stmt) (inner, outer) = undefined

