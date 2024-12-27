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
convertToSimple (Int _)  = SimpleInt
convertToSimple (Str _)  = SimpleString
convertToSimple (Bool _) = SimpleBool
convertToSimple (Void _) = SimpleVoid

extractIdent :: Ident -> String
extractIdent (Ident s) = s

checkIfTypesTheSame :: SimpleType -> SimpleType -> Bool
checkIfTypesTheSame a b = a == b

insertMultiple :: Ord k => [(k, v)] -> Map k v -> Map k v
insertMultiple kvs m = foldl (\acc (k, v) -> Map.insert k v acc) m kvs

data Env = Env {
  variableToType   :: Map.Map String SimpleType,
  funArgumentTypes :: Map.Map String [SimpleType],
  funReturnTypes   :: Map.Map String SimpleType,
  returnFlag       :: (Bool, SimpleType)
 }

predefinedPrintsAndReads :: [(String, [SimpleType], SimpleType)]
predefinedPrintsAndReads =
  [ ("printInt"   , [SimpleInt]   , SimpleVoid)
  , ("printString", [SimpleString], SimpleVoid)
  , ("printBool"  , [SimpleBool]  , SimpleVoid)
  , ("readInt"    , []            , SimpleInt)
  , ("readString" , []            , SimpleString)
  , ("readBool"   , []            , SimpleBool)
  ]

emptyEnv :: Env
emptyEnv = Env {
  variableToType   = Map.empty,
  funArgumentTypes = Map.fromList [(name, args) | (name, args, _) <- predefinedPrintsAndReads],
  funReturnTypes   = Map.fromList [(name, ret)  | (name, _, ret) <- predefinedPrintsAndReads],
  returnFlag       = (False, SimpleVoid)
}

checkIfAlreadyDeclaredInEnv :: String -> Env -> Either String Bool
checkIfAlreadyDeclaredInEnv var env =
  case Map.lookup var (variableToType env) of
    Just _  -> Right True
    Nothing -> Left $ "Variable " ++ var ++ " already declared"

instance Show SimpleType where
  show SimpleInt    = "int"
  show SimpleBool   = "bool"
  show SimpleString = "string"
  show SimpleVoid   = "void"

instance Show Env where
  show (Env varTypes funArgTypes funRetTypes flag) =
    "Env { varTypes = " ++ show varTypes
    ++ ", funArgTypes = " ++ show funArgTypes
    ++ ", funRetTypes = " ++ show funRetTypes
    ++ ", flag = " ++ show flag ++ " }"

resetReturnFlag :: Env -> Env
resetReturnFlag env = env { returnFlag = (False, SimpleVoid) }

type TypecheckerResult a = StateT Env (ExceptT String IO) a

class Typechecker a where
  evalType :: a -> TypecheckerResult SimpleType

typecheck :: Program -> IO (Either String SimpleType)
typecheck program = runExceptT $ evalStateT (evalType program) emptyEnv

showPosition :: BNFC'Position -> String
showPosition (Just (l, c)) = "line " ++ show l ++ ", column " ++ show c
showPosition Nothing       = "unknown position"

showIdent :: Ident -> String
showIdent (Ident s) = s

instance Typechecker Block where
  evalType (IBlock pos stmts) = do
    env <- get
    mapM_ evalType stmts
    env' <- get
    put $ env' {
      variableToType   = variableToType env,
      funArgumentTypes = funArgumentTypes env,
      funReturnTypes   = funReturnTypes env
    }
    return SimpleVoid

instance Typechecker Expr where
  evalType (EVar pos ident) = do
    env <- get
    case Map.lookup (extractIdent ident) (variableToType env) of
      Just t  -> return t
      Nothing -> throwError $ "Variable " ++ show (extractIdent ident)
                              ++ " at " ++ showPosition pos
                              ++ " not declared"

  evalType (ELitInt _ _)     = return SimpleInt
  evalType (ELitTrue _)      = return SimpleBool
  evalType (ELitFalse _)     = return SimpleBool
  evalType (EString _ _)     = return SimpleString

  evalType (EApp pos ident exprs) = do
    env <- get
    -- First, check if the function is declared at all (i.e. in funReturnTypes).
    case Map.lookup (extractIdent ident) (funReturnTypes env) of
      Nothing ->
        throwError $ "Function " ++ showIdent ident
                    ++ " at " ++ showPosition pos
                    ++ " not declared"
      Just retType -> do
        -- Next, check its argument types in funArgumentTypes.
        case Map.lookup (extractIdent ident) (funArgumentTypes env) of
          Nothing ->
            if null exprs
              then return retType
              else throwError $ "Function " ++ showIdent ident
                              ++ " at " ++ showPosition pos
                              ++ " called with wrong number of argumentss"
          Just argTypes -> do
            -- Check if the number of arguments matches.
            if length argTypes /= length exprs
              then throwError $ "Function " ++ showIdent ident
                              ++ " at " ++ showPosition pos
                              ++ " called with wrong number of argumentsss"
              else do
                -- Check if the argument types match.
                exprTypes <- mapM evalType exprs
                if and $ zipWith checkIfTypesTheSame exprTypes argTypes
                  then return retType
                  else throwError $ "Function " ++ showIdent ident
                                  ++ " at " ++ showPosition pos
                                  ++ " called with wrong argument types"


  evalType (Neg pos expr) = do
    t <- evalType expr
    if checkIfTypesTheSame t SimpleInt
      then return SimpleInt
      else throwError $ "Negating non-integer expression at "
                       ++ showPosition pos

  evalType (Not pos expr) = do
    t <- evalType expr
    if checkIfTypesTheSame t SimpleBool
      then return SimpleBool
      else throwError $ "Negating non-boolean expression at "
                       ++ showPosition pos

  evalType (EMul pos expr1 op expr2) = do
    t1 <- evalType expr1
    t2 <- evalType expr2
    if checkIfTypesTheSame t1 SimpleInt
       && checkIfTypesTheSame t2 SimpleInt
      then return SimpleInt
      else throwError $ "Multiplying non-integer expressions at "
                       ++ showPosition pos

  evalType (EAdd pos expr1 _ expr2) = do
    t1 <- evalType expr1
    t2 <- evalType expr2
    if ( checkIfTypesTheSame t1 SimpleInt
         && checkIfTypesTheSame t2 SimpleInt
       ) || ( checkIfTypesTheSame t1 SimpleString
              && checkIfTypesTheSame t2 SimpleString
            )
      then
        if checkIfTypesTheSame t1 SimpleString
           && checkIfTypesTheSame t2 SimpleString
          then return SimpleString
          else return SimpleInt
      else throwError $ "Adding expressions of incompatible types at "
                       ++ showPosition pos

  evalType (ERel pos expr1 _ expr2) = do
    t1 <- evalType expr1
    t2 <- evalType expr2
    if (checkIfTypesTheSame t1 SimpleInt
        && checkIfTypesTheSame t2 SimpleInt)
       || (checkIfTypesTheSame t1 SimpleBool
           && checkIfTypesTheSame t2 SimpleBool)
       || (checkIfTypesTheSame t1 SimpleString
           && checkIfTypesTheSame t2 SimpleString)
      then return SimpleBool
      else throwError $ "Comparing incompatible expressions at "
                       ++ showPosition pos

  evalType (EAnd pos expr1 expr2) = do
    t1 <- evalType expr1
    t2 <- evalType expr2
    if checkIfTypesTheSame t1 SimpleBool
       && checkIfTypesTheSame t2 SimpleBool
      then return SimpleBool
      else throwError $ "Logical AND on non-boolean expressions at "
                       ++ showPosition pos

  evalType (EOr pos expr1 expr2) = do
    t1 <- evalType expr1
    t2 <- evalType expr2
    if checkIfTypesTheSame t1 SimpleBool
       && checkIfTypesTheSame t2 SimpleBool
      then return SimpleBool
      else throwError $ "Logical OR on non-boolean expressions at "
                       ++ showPosition pos

instance Typechecker TopDef where
  evalType (FnDef pos retType ident args block) = do
    env <- get
    -- Insert function (besides it's name and type) into the environment
    let newEnv = env
          { variableToType = insertMultiple
               [ ( extractIdent varIdent
                 , convertToSimple t
                 )
               | arg <- args
               , let (IArg _ t' varIdent') = arg
               , let t       = t'
               , let varIdent = varIdent'
               ]
               (variableToType env)
               
          , returnFlag = returnFlag env
          }
    put newEnv

    -- Evaluate the function body
    evalType block

    -- Check return type consistency
    if convertToSimple retType == SimpleVoid
      then return SimpleVoid
      else do
        envWithReturnFlag <- get
        put $ resetReturnFlag envWithReturnFlag
        if fst $ returnFlag envWithReturnFlag
          then
            if snd (returnFlag envWithReturnFlag)
               == convertToSimple retType
              then return $ convertToSimple retType
              else throwError $ "Function "
                                ++ showIdent ident
                                ++ " at "
                                ++ showPosition pos
                                ++ " returns wrong type"
          else throwError $ "Function "
                            ++ showIdent ident
                            ++ " at "
                            ++ showPosition pos
                            ++ " does not return"

addFunctionToEnv :: TopDef -> TypecheckerResult SimpleType
addFunctionToEnv (FnDef pos retType ident args block) = do
    env <- get
    let newEnv = env
          { 
            -- variableToType = insertMultiple
            --    [ ( extractIdent varIdent
            --      , convertToSimple t
            --      )
            --    | arg <- args
            --    , let (IArg _ t' varIdent') = arg
            --    , let t       = t'
            --    , let varIdent = varIdent'
            --    ]
            --    (variableToType env)

          funReturnTypes = Map.insert
               (extractIdent ident)
               (convertToSimple retType)
               (funReturnTypes env)

          , funArgumentTypes = Map.insert
               (extractIdent ident)
               [ convertToSimple t'
               | IArg _ t' _ <- args
               ]
               (funArgumentTypes env)

          -- , returnFlag = returnFlag env
          }
    put newEnv
    return SimpleVoid

instance Typechecker Program where
  evalType (IProgram pos topDefs) = do
    -- First, add all top-level functions to the environment
    mapM_ addFunctionToEnv topDefs
    -- Then, actually typecheck them
    mapM_ evalType topDefs

    -- Finally, check for a valid `main` function
    env <- get
    case Map.lookup "main" (funArgumentTypes env) of
      Just [] ->
        case Map.lookup "main" (funReturnTypes env) of
          Just SimpleInt -> return SimpleInt
          Just _ -> throwError $ "Function main at "
                                 ++ showPosition pos
                                 ++ " returns wrong type"
          Nothing -> throwError $ "Function main at "
                                  ++ showPosition pos
                                  ++ " not declared"
      _ -> throwError $ "Function main at "
                        ++ showPosition pos
                        ++ " not declared"

instance Typechecker Arg where
  -- Since we no longer have VarArg, we only match IArg
  evalType (IArg pos t _) = return $ convertToSimple t

isELitTrue :: Expr -> Bool
isELitTrue (ELitTrue _) = True
isELitTrue _            = False

isELitFalse :: Expr -> Bool
isELitFalse (ELitFalse _) = True
isELitFalse _             = False

instance Typechecker Stmt where
  evalType (Decr pos ident) = do
    env <- get
    case Map.lookup (extractIdent ident) (variableToType env) of
      Just SimpleInt -> return SimpleInt
      Just _ -> throwError $ "Decrementing non-integer variable "
                             ++ show (extractIdent ident)
                             ++ " at "
                             ++ showPosition pos
      Nothing -> throwError $ "Variable "
                              ++ show (extractIdent ident)
                              ++ " at "
                              ++ showPosition pos
                              ++ " not declared (decr)"

  evalType (Ass pos ident expr) = do
    env <- get
    exprType <- evalType expr
    case Map.lookup (extractIdent ident) (variableToType env) of
      Just a -> if a == exprType
                  then return SimpleVoid
                  else throwError $ "Wrong type in assignment at "
                                   ++ showPosition pos
      Nothing -> throwError $ "Variable "
                              ++ show (extractIdent ident)
                              ++ " at "
                              ++ showPosition pos
                              ++ " not declared (ass)"

  evalType (Cond pos expr stmt) = do
    exprType <- evalType expr
    if checkIfTypesTheSame exprType SimpleBool
      then
        if not (isELitFalse expr)
          then evalType stmt
          else return SimpleVoid
      else throwError $ "Condition in if statement at "
                       ++ showPosition pos
                       ++ " is not a boolean expression"

  evalType (CondElse pos expr stmt1 stmt2) = do
    exprType <- evalType expr
    if checkIfTypesTheSame exprType SimpleBool
      then
        if isELitTrue expr
          then evalType stmt1
          else evalType stmt2
      else throwError $ "Condition in if-else statement at "
                       ++ showPosition pos
                       ++ " is not a boolean expression"

  evalType (While pos expr stmt) = do
    exprType <- evalType expr
    if checkIfTypesTheSame exprType SimpleBool
      then evalType stmt
      else throwError $ "Condition in while statement at "
                       ++ showPosition pos
                       ++ " is not a boolean expression"

  evalType (SExp _ expr) =
    evalType expr

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
      Just _ -> throwError $ "Incrementing non-integer variable "
                             ++ show (extractIdent ident)
                             ++ " at "
                             ++ showPosition pos
      Nothing -> throwError $ "Variable "
                              ++ show (extractIdent ident)
                              ++ " at "
                              ++ showPosition pos
                              ++ " not declared"

  evalType (Empty _) =
    return SimpleVoid

  evalType (BStmt _ block) =
    evalType block

  evalType (Decl pos declType items) = do
    env <- get
    -- We'll accumulate the updated map of variables as we go.
    -- foldM is handy since we're doing effects (evalType expr for Init).
    newMap <- foldM
      (\acc declItem ->
        case declItem of
          NoInit itemPos ident -> do
            -- Check if already declared
            case Map.lookup (extractIdent ident) acc of
              Just _ ->
                throwError $ "Variable " ++ show (extractIdent ident)
                            ++ " at " ++ showPosition itemPos
                            ++ " already declared"
              Nothing -> do
                -- Insert variable with type from declType
                return (Map.insert (extractIdent ident)
                                    (convertToSimple declType)
                                    acc)

          Init itemPos ident expr -> do
            -- Check if already declared
            exprType <- evalType expr
            -- Option A: enforce that the exprType matches declType
            let declaredType = convertToSimple declType
            if checkIfTypesTheSame exprType declaredType
              then return (Map.insert (extractIdent ident) declaredType acc)
              else throwError $ "Variable " ++ show (extractIdent ident)
                                ++ " at " ++ showPosition itemPos
                                ++ " has type " ++ show declaredType
                                ++ " but is initialized with "
                                ++ show exprType
      )
      (variableToType env)
      items

    -- Put the new map of variables into the environment
    let newEnv = env { variableToType = newMap }
    put newEnv
    return SimpleVoid


  evalType (FVInit pos topDef) =
    evalType topDef        


