{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Backend where

import Data.Map (Map)
import qualified Data.Map as Map

import Parser.Abs
import Types
import Aux
import State
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import GHC.TopHandler (runIO)


-- TODO: 
-- phi (variable overshadowing)
-- function calls
-- if, if else, while
-- block
-- read, print

class Compiler a where
  generateLLVM :: a -> CompilerM RegisterAndType -- return in which register the result is stored

runCompiler :: Program -> IO RegisterAndType
runCompiler program = runIO $ evalStateT (generateLLVM program) initialState


instance Compiler Program where
  generateLLVM :: Program -> CompilerM RegisterAndType
  generateLLVM (Program _ topDefs) = do
    topDefsLLVM <- mapM generateLLVM topDefs
    genLLVM <- getAllBasicBlocksGenLLVM
    mapM_ (liftIO . print) genLLVM
    return dummyReturnRegisterAndType


instance Compiler TopDef where -- IMO no blocks generation is required - just in function eapp I'll need to create a fresh env only with the arguments
  generateLLVM :: TopDef -> CompilerM RegisterAndType
  generateLLVM (FnDef _ retType ident args block) = do
    let argTypes = map (\(Arg _ t _) -> t) args
    let funValue = EVFun (bnfcTypeToLLVMType retType) ident args block
    modify $ \s -> s { identToFunSig = Map.insert ident funValue (identToFunSig s) }
    label <- getNextLabelAndIncrement -- get new label for function
    modify $ \s -> s { currBasicBlockLabel = label }
    insertEmptyBasicBlock label
    addGenLLVM $ IFunPr funValue
    addGenLLVM $ ILabel label
    blockLastReg <- generateLLVM block
    -- do phi?? - maybe not needed
    addGenLLVM IFunEp
    return dummyReturnRegisterAndType


instance Compiler Block where
  generateLLVM :: Block -> CompilerM RegisterAndType
  generateLLVM (Block _ stmts) = do
    -- create a new block by adding a label and registering the block in the state
    label <- getNextLabelAndIncrement
    modify $ \s -> s { currBasicBlockLabel = label }
    insertEmptyBasicBlock label
    liftIO $ print (show label ++ "label" ++ show stmts)
    addGenLLVM $ ILabel label
    oldState <- get
    -- TODO: add phi instructions for variables that were changed in the predecessor block
    -- and adjust the state based on the old one
    -- insertEmptyBasicBlock label

    mapM_ generateLLVM stmts
    return dummyReturnRegisterAndType


instance Compiler Expr where
  generateLLVM :: Expr -> CompilerM RegisterAndType
  generateLLVM (EVar _ ident) = do
    identToReg <- gets identToRegisterAndType
    case Map.lookup ident identToReg of
      Just (reg, typ) -> return (reg, typ)
      Nothing -> error $ "Variable " ++ extractIdent ident ++ " not found"

  generateLLVM (ELitInt _ i) = do
    reg <- getNextRegisterAndIncrement
    addGenLLVM $ IAss (EVReg reg) (EVInt $ fromIntegral i)
    return (reg, TVInt)

  generateLLVM (ELitTrue _) = do
    reg <- getNextRegisterAndIncrement
    addGenLLVM $ IAss (EVReg reg) (EVBool True)
    return (reg, TVBool)

  generateLLVM (ELitFalse _) = do
    reg <- getNextRegisterAndIncrement
    addGenLLVM $ IAss (EVReg reg) (EVBool False)
    return (reg, TVBool)

  generateLLVM (EString _ s) = do
    reg <- getNextRegisterAndIncrement
    addGenLLVM $ IAss (EVReg reg) (EVString s)
    return (reg, TVString)

  generateLLVM (Neg _ expr) = do
    (exprReg, exprRegType) <- generateLLVM expr
    resultReg <- getNextRegisterAndIncrement
    addGenLLVM $ IAss (EVReg resultReg) (EVReg exprReg)
    return (resultReg, exprRegType)

  generateLLVM (Not _ expr) = do
    (exprReg, exprRegType) <- generateLLVM expr
    resultReg <- getNextRegisterAndIncrement
    addGenLLVM $ IAss (EVReg resultReg) (EVReg exprReg)
    return (resultReg, exprRegType)

  generateLLVM (EMul _ expr1 mulOp expr2) = do
    (exprReg1, exprRegType1) <- generateLLVM expr1
    (exprReg2, exprRegType2) <- generateLLVM expr2
    resultReg <- getNextRegisterAndIncrement
    let binOp = case mulOp of
          Times _ -> BMul
          Div _ -> BDiv
          Mod _ -> BMod
    addGenLLVM $ IBinOp (EVReg resultReg) (EVReg exprReg1) (EVReg exprReg2) binOp
    return (resultReg, exprRegType1)

  generateLLVM (EAdd _ expr1 addOp expr2) = do
    (exprReg1, exprRegType1) <- generateLLVM expr1
    (exprReg2, exprRegType2) <- generateLLVM expr2
    resultReg <- getNextRegisterAndIncrement
    let binOp = case addOp of
          Plus _ -> BAdd
          Minus _ -> BSub
    addGenLLVM $ IBinOp (EVReg resultReg) (EVReg exprReg1) (EVReg exprReg2) binOp
    return (resultReg, exprRegType1)

  generateLLVM (ERel _ expr1 oper expr2) = do
    (exprReg1, exprRegType1) <- generateLLVM expr1
    (exprReg2, exprRegType2) <- generateLLVM expr2
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

  generateLLVM (EAnd _ expr1 expr2) = do
    (exprReg1, exprRegType1) <- generateLLVM expr1
    (exprReg2, exprRegType2) <- generateLLVM expr2
    resultReg <- getNextRegisterAndIncrement
    addGenLLVM $ IBinOp (EVReg resultReg) (EVReg exprReg1) (EVReg exprReg2) BAnd
    return (resultReg, TVBool)

  generateLLVM (EOr _ expr1 expr2) = do
    (exprReg1, exprRegType1) <- generateLLVM expr1
    (exprReg2, exprRegType2) <- generateLLVM expr2
    resultReg <- getNextRegisterAndIncrement
    addGenLLVM $ IBinOp (EVReg resultReg) (EVReg exprReg1) (EVReg exprReg2) BOr
    return (resultReg, TVBool)

  generateLLVM (EApp _ ident exprs) = undefined
-- add new env

generateIncDec :: Ident -> DBinOp -> CompilerM (Register, LLVMType)
generateIncDec ident op = do
    (identReg, identRegType) <- lookupIdentRegisterAndType ident
    newReg <- getNextRegisterAndIncrement
    addGenLLVM $ IBinOp (EVReg newReg) (EVReg identReg) (EVInt 1) op
    insertIdentRegisterAndType ident newReg identRegType
    return (newReg, identRegType)


instance Compiler Stmt where
  generateLLVM :: Stmt -> CompilerM RegisterAndType
  generateLLVM (Ass _ ident expr) = do
    (exprReg, exprRegType) <- generateLLVM expr   -- generate code for expression and store result in register
    lhsReg <- getNextRegisterAndIncrement         -- get new free register for lhs
    insertInstrToCurrBasicBlock $ IAss (EVReg lhsReg) (EVReg exprReg) -- generate LLVM instruction
    insertIdentRegisterAndType ident exprReg exprRegType
    b <- isIdentInVarsDeclaredInCurrBlock ident
    liftIO $ print b
    curr <- getCurrentBasicBlockLabel
    liftIO $ print curr
    (if b then return (lhsReg, exprRegType) else (do
      insertVarChangedFromPredBlock ident (exprReg, exprRegType)
      return (lhsReg, exprRegType)))

  generateLLVM (Decl _ itemsType items) = do
    foldM_
      (\_ declItem -> case declItem of
        NoInit _ ident -> do
          reg <- getNextRegisterAndIncrement
          let llvmItemsType = bnfcTypeToLLVMType itemsType
          addGenLLVM $ IAss (EVReg reg) (getValueDefaultInit llvmItemsType)
          insertIdentRegisterAndType ident reg llvmItemsType
          insertVarDeclaredInCurrBlock ident
          return ()
        Init _ ident expr -> do
          (lhsReg, exprRegType) <- generateLLVM (Ass BNFC'NoPosition ident expr)
          insertVarDeclaredInCurrBlock ident
          return ()
      )
      ()
      items
    return dummyReturnRegisterAndType

  generateLLVM (Ret _ expr) = do
    (exprReg, exprRegType) <- generateLLVM expr
    addGenLLVM $ IFunRet (EVReg exprReg) exprRegType
    return (exprReg, exprRegType)

  generateLLVM (VRet _) = do
    voidReg <- getNextRegisterAndIncrement
    addGenLLVM $ IFunRet (EVReg voidReg) TVVoid
    return (voidReg, TVVoid)

  generateLLVM (Empty _) = return dummyReturnRegisterAndType

  generateLLVM (BStmt _ block) = generateLLVM block -- TODO: overshadowing, probably set predBlockLabel

  generateLLVM (Incr _ ident) = generateIncDec ident BAdd

  generateLLVM (Decr _ ident) = generateIncDec ident BSub

  generateLLVM (SExp _ expr) = generateLLVM expr

  generateLLVM (Cond _ expr stmt) = do
    (exprReg, exprRegType) <- generateLLVM expr

    labelTrue <- getNextLabelAndIncrement -- empty blocks generation
    labelEnd <- getNextLabelAndIncrement
    insertEmptyBasicBlock labelTrue
    insertEmptyBasicBlock labelEnd

    insertInstrToCurrBasicBlock $ IBr (EVReg exprReg) labelTrue labelEnd
    currBB <- getCurrentBasicBlockLabel

    -- generate code for true branch
    addGenLLVM $ ILabel labelTrue
    oldState <- get
    setPredecessorLabel currBB
    setcurrBasicBlockLabel labelTrue
    generateLLVM stmt
    newState <- get
    addGenLLVM $ IBrJump labelEnd

    -- generate code for end of if statement - Phi. We should generate
    -- phi for all variables that were changed in the true branch
    addGenLLVM $ ILabel labelEnd
    liftIO $ print $ varsChangedFromPredBlock (basicBlocks newState Map.! labelTrue)
    liftIO $ print labelTrue
    mapM_
      (\(ident, (reg, typ)) -> do
        (predReg, predType) <- lookupIdentRegisterAndTypeInOldState oldState ident
        liftIO $ print "GENERATING PHI"
        newReg <- getNextRegisterAndIncrement
        addGenLLVM $ IPhi (EVReg newReg) (EVReg predReg, currBB) (EVReg reg, labelTrue)
        insertIdentRegisterAndType ident newReg typ
      )
      (Map.toList $ varsChangedFromPredBlock (basicBlocks newState Map.! labelTrue))

    return dummyReturnRegisterAndType

  generateLLVM (CondElse _ expr s1 s2)     = undefined
  generateLLVM (While _ expr stmt)         = undefined

