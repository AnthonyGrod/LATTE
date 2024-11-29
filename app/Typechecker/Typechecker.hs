{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Typechecker.Typechecker where

import Control.Monad.IO.Class (liftIO)
import Data.Map (Map)
import qualified Data.Map as Map

import Parser.Abs
import Control.Monad.State
import Control.Monad.Except

data SimpleType = SimpleInt | SimpleBool | SimpleString | SimpleVoid deriving Eq

convertToSimple :: Type -> SimpleType
convertToSimple (Int _) = SimpleInt
convertToSimple (Str _) = SimpleString
convertToSimple (Bool _) = SimpleBool
convertToSimple (Void _) = SimpleVoid

extractIdent :: Ident -> String
extractIdent (Ident s) = s

checkIfTypesTheSame :: SimpleType -> SimpleType -> Bool
checkIfTypesTheSame a b = a == b

insertMultiple :: Ord k => [(k, v)] -> Map k v -> Map k v
insertMultiple kvs m = foldl (\acc (k, v) -> Map.insert k v acc) m kvs

data Env = Env {
  variableToType :: Map.Map String SimpleType,
  funArgumentTypes :: Map.Map String [SimpleType],
  funReturnTypes :: Map.Map String SimpleType,
  returnFlag :: (Bool, SimpleType)
 }

predefinedPrints :: [(String, [SimpleType], SimpleType)]
predefinedPrints = [("printInt", [SimpleInt], SimpleVoid), ("printString", [SimpleString], SimpleVoid), ("printBool", [SimpleBool], SimpleVoid)]

emptyEnv :: Env
emptyEnv = Env {
  variableToType = Map.empty,
  funArgumentTypes = Map.fromList [(name, args) | (name, args, _) <- predefinedPrints],
  funReturnTypes = Map.fromList [(name, ret) | (name, _, ret) <- predefinedPrints],
  returnFlag = (False, SimpleVoid)
}

instance Show SimpleType where
  show SimpleInt = "int"
  show SimpleBool = "bool"
  show SimpleString = "string"
  show SimpleVoid = "void"

instance Show Env where
  show (Env varTypes funArgTypes funRetTypes flag) = "Env { varTypes = " ++ show varTypes ++ ", funArgTypes = " ++ show funArgTypes ++ ", funRetTypes = " ++ show funRetTypes ++ ", flag = " ++ show flag ++ " }"

resetReturnFlag :: Env -> Env
resetReturnFlag env = env { returnFlag = (False, SimpleVoid) }

type TypecheckerResult a = StateT Env (ExceptT String IO) a

class Typechecker a where
  evalType :: a -> TypecheckerResult SimpleType

typecheck :: Program -> IO (Either String SimpleType)
typecheck program = runExceptT $ evalStateT (evalType program) emptyEnv

showPosition :: BNFC'Position -> String
showPosition (Just (l, c)) = "line " ++ show l ++ ", column " ++ show c

showIdent :: Ident -> String
showIdent (Ident s) = s


instance Typechecker Block where
  evalType (IBlock pos stmts) = do
    env <- get
    mapM_ evalType stmts
    env' <- get
    put $ env' {
      variableToType = variableToType env,
      funArgumentTypes = funArgumentTypes env,
      funReturnTypes = funReturnTypes env }
    return SimpleVoid


instance Typechecker Expr where
  evalType :: Expr -> TypecheckerResult SimpleType
  evalType (EVar pos ident) = do
    env <- get
    case Map.lookup (extractIdent ident) (variableToType env) of
      Just t -> return t
      Nothing -> throwError $ "Variable " ++ show (extractIdent ident) ++ " at " ++ showPosition pos ++ " not declared"

  evalType (ELitInt _ _) = return SimpleInt
  evalType (ELitTrue _) = return SimpleBool
  evalType (ELitFalse _) = return SimpleBool
  evalType (EString _ _) = return SimpleString

  evalType (EApp pos ident exprs) = do
    env <- get
    case Map.lookup (extractIdent ident) (funArgumentTypes env) of
      Just argTypes -> do
        exprs' <- mapM evalType exprs
        if argTypes == exprs'
          then case Map.lookup (extractIdent ident) (funReturnTypes env) of
            Just t -> return t
            Nothing -> throwError $ "Function " ++ showIdent ident ++ " at " ++ showPosition pos ++ " not declared"
          else throwError $ "Function " ++ showIdent ident ++ " at " ++ showPosition pos ++ " called with wrong arguments"
      Nothing -> throwError $ "Function " ++ showIdent ident ++ " at " ++ showPosition pos ++ " not declared"

  evalType (Neg pos expr) = do
    t <- evalType expr
    if checkIfTypesTheSame t SimpleInt
      then return SimpleInt
      else throwError $ "Negating non-integer expression at " ++ showPosition pos

  evalType (Not pos expr) = do
    t <- evalType expr
    if checkIfTypesTheSame t SimpleBool
      then return SimpleBool
      else throwError $ "Negating non-boolean expression at " ++ showPosition pos

  evalType (EMul pos expr1 op expr2) = do
    t1 <- evalType expr1
    t2 <- evalType expr2
    if checkIfTypesTheSame t1 SimpleInt && checkIfTypesTheSame t2 SimpleInt
      then return SimpleInt
      else throwError $ "Multiplying non-integer expressions at " ++ showPosition pos

  evalType (EAdd pos expr1 _ expr2) = do
    t1 <- evalType expr1
    t2 <- evalType expr2
    if (checkIfTypesTheSame t1 SimpleInt && checkIfTypesTheSame t2 SimpleInt) || (checkIfTypesTheSame t1 SimpleString && checkIfTypesTheSame t2 SimpleString)
      then 
        if checkIfTypesTheSame t1 SimpleString && checkIfTypesTheSame t2 SimpleString
          then return SimpleString
          else return SimpleInt
      else throwError $ "Adding non-integer expressions at " ++ showPosition pos

  evalType (ERel pos expr1 _ expr2) = do
    t1 <- evalType expr1
    t2 <- evalType expr2
    if (checkIfTypesTheSame t1 SimpleInt && checkIfTypesTheSame t2 SimpleInt)
      || (checkIfTypesTheSame t1 SimpleBool && checkIfTypesTheSame t2 SimpleBool)
      || (checkIfTypesTheSame t1 SimpleString && checkIfTypesTheSame t2 SimpleString)
      then return SimpleBool
      else throwError $ "Comparing non-integer expressions at " ++ showPosition pos

  evalType (EAnd pos expr1 expr2) = do
    t1 <- evalType expr1
    t2 <- evalType expr2
    if checkIfTypesTheSame t1 SimpleBool && checkIfTypesTheSame t2 SimpleBool
      then return SimpleBool
      else throwError $ "Anding non-boolean expressions at " ++ showPosition pos

  evalType (EOr pos expr1 expr2) = do
    t1 <- evalType expr1
    t2 <- evalType expr2
    if checkIfTypesTheSame t1 SimpleBool && checkIfTypesTheSame t2 SimpleBool
      then return SimpleBool
      else throwError $ "Oring non-boolean expressions at " ++ showPosition pos


instance Typechecker TopDef where
  evalType :: TopDef -> TypecheckerResult SimpleType
  evalType (FnDef pos retType ident args block) = do
    env <- get
    -- let newEnv = env {
    --   variableToType = insertMultiple [(extractIdent varIdent, convertToSimple t) | arg <- args, let (t, varIdent) = case arg of
    --                                                                                                          IArg _ t' varIdent' -> (t', varIdent')
    --                                                                                                          VarArg _ t' varIdent' -> (t', varIdent')] (variableToType env),
    --   funReturnTypes = Map.insert (extractIdent ident) (convertToSimple retType) (funReturnTypes env),
    --   funArgumentTypes = Map.insert (extractIdent ident) (map (\arg -> case arg of
    --                                                                      IArg _ t _ -> convertToSimple t
    --                                                                      VarArg _ t _ -> convertToSimple t) args) (funArgumentTypes env),
    --   returnFlag = returnFlag env }
    -- put newEnv
    evalType block
    envWithReturnFlag <- get
    put $ resetReturnFlag envWithReturnFlag
    if fst $ returnFlag envWithReturnFlag then
      if snd (returnFlag envWithReturnFlag) == convertToSimple retType
        then return $ convertToSimple retType
        else throwError $ "Function " ++ showIdent ident ++ " at " ++ showPosition pos ++ " returns wrong type"
    else throwError $ "Function " ++ showIdent ident ++ " at " ++ showPosition pos ++ " does not return"


  evalType (VarDef pos declType ident expr) = do
    env <- get
    exprType <- evalType expr
    if not $ checkIfTypesTheSame (convertToSimple declType) exprType
      then throwError $ "Variable " ++ show (extractIdent ident) ++ " at " ++ showPosition pos ++ " initialized with wrong type"
    else do
      let newEnv = env { variableToType = Map.insert (extractIdent ident) (convertToSimple declType) (variableToType env) }
      put newEnv
      return SimpleVoid


addFunctionToEnv :: TopDef -> TypecheckerResult SimpleType
addFunctionToEnv (FnDef pos retType ident args block) = do
    env <- get
    let newEnv = env {
      variableToType = insertMultiple [(extractIdent varIdent, convertToSimple t) | arg <- args, let (t, varIdent) = case arg of
                                                                                                             IArg _ t' varIdent' -> (t', varIdent')
                                                                                                             VarArg _ t' varIdent' -> (t', varIdent')] (variableToType env),
      funReturnTypes = Map.insert (extractIdent ident) (convertToSimple retType) (funReturnTypes env),
      funArgumentTypes = Map.insert (extractIdent ident) (map (\arg -> case arg of
                                                                         IArg _ t _ -> convertToSimple t
                                                                         VarArg _ t _ -> convertToSimple t) args) (funArgumentTypes env),
      returnFlag = returnFlag env }
    put newEnv
    return SimpleVoid


instance Typechecker Program where
  evalType :: Program -> TypecheckerResult SimpleType
  evalType (IProgram pos topDefs) = do
    mapM_ addFunctionToEnv topDefs
    mapM_ evalType topDefs
    env <- get
    case Map.lookup "main" (funArgumentTypes env) of
      Just [] -> case Map.lookup "main" (funReturnTypes env) of
        Just SimpleInt -> return SimpleInt
        Just _ -> throwError $ "Function main at " ++ showPosition pos ++ " returns wrong type"
        Nothing -> throwError $ "Function main at " ++ showPosition pos ++ " not declared"
      _ -> throwError $ "Function main at " ++ showPosition pos ++ " not declared"


instance Typechecker Arg where
  evalType :: Arg -> TypecheckerResult SimpleType
  evalType (IArg pos t _) = return $ convertToSimple t
  evalType (VarArg pos t _) = return $ convertToSimple t


instance Typechecker Stmt where
  evalType :: Stmt -> TypecheckerResult SimpleType
  evalType (Decr pos ident) = do
    env <- get
    case Map.lookup (extractIdent ident) (variableToType env) of
      Just SimpleInt -> return SimpleInt
      Just _ -> throwError $ "Decrementing non-integer variable " ++ show (extractIdent ident) ++ " at " ++ showPosition pos
      Nothing -> throwError $ "Variable " ++ show (extractIdent ident) ++ " at " ++ showPosition pos ++ " not declared (decr)"

  evalType (Ass pos ident expr) = do
    env <- get
    exprType <- evalType expr
    case Map.lookup (extractIdent ident) (variableToType env) of
      Just a -> if a == exprType then return SimpleVoid else throwError $ "Wrong type in assignment at " ++ showPosition pos
      Nothing -> throwError $ "Variable " ++ show (extractIdent ident) ++ " at " ++ showPosition pos ++ " not declared (decr)"

  evalType (Cond pos expr stmt) = do
    exprType <- evalType expr
    if (checkIfTypesTheSame exprType SimpleBool)
      then evalType stmt
      else throwError $ "Condition in if statement at " ++ showPosition pos ++ " is not a boolean expression"

  evalType (CondElse pos expr stmt1 stmt2) = do
    exprType <- evalType expr
    if checkIfTypesTheSame exprType SimpleBool
      then do
        evalType stmt1
        evalType stmt2
      else throwError $ "Condition in if-else statement at " ++ showPosition pos ++ " is not a boolean expression"

  evalType (While pos expr stmt) = do
    exprType <- evalType expr
    if checkIfTypesTheSame exprType SimpleBool
      then evalType stmt
      else throwError $ "Condition in while statement at " ++ showPosition pos ++ " is not a boolean expression"

  evalType (SExp _ expr) = evalType expr

  evalType (Ret pos expr) = do
    env <- get
    exprType <- evalType expr
    put $ env { returnFlag = (True, exprType) }
    return SimpleVoid

  evalType (VRet pos) = do
    env <- get
    put $ env { returnFlag = (True, SimpleVoid) }
    return SimpleVoid

  evalType (Incr pos ident) = do
    env <- get
    case Map.lookup (extractIdent ident) (variableToType env) of
      Just SimpleInt -> return SimpleInt
      Just _ -> throwError $ "Incrementing non-integer variable " ++ show (extractIdent ident) ++ " at " ++ showPosition pos
      Nothing -> throwError $ "Variable " ++ show (extractIdent ident) ++ " at " ++ showPosition pos ++ " not declared"

  evalType (Empty _) = return SimpleVoid

  evalType (BStmt _ block) = evalType block

  evalType (Decl pos declType items) = do
    env <- get
    let newEnv = env { variableToType = foldl (\acc (NoInit _ ident) -> Map.insert (extractIdent ident) (convertToSimple declType) acc) (variableToType env) items }
    put newEnv
    return SimpleVoid

  evalType (FVInit pos topDef) = do
    evalType topDef

