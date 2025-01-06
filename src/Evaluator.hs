{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}


module Evaluator.Evaluator where

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Except

import Parser.Abs
import Foreign (new)

data Value =
  EVInt Integer |
  EVBool Bool |
  EVString String |
  EVVoid |
  EVFun Type Env Ident [Arg] Block

getValueDefaultInit :: Type -> Value
getValueDefaultInit (Int _) = EVInt 0
getValueDefaultInit (Str _) = EVString ""
getValueDefaultInit (Bool _) = EVBool False
getValueDefaultInit _ = EVVoid

getIntegerValue :: Value -> Integer
getIntegerValue (EVInt i) = i
getIntegerValue _ = error "Not an integer value"

instance Num Value where
  (+) (EVInt i1) (EVInt i2) = EVInt (i1 + i2)
  (+) _ _ = error "Addition of non-integer values"
  (*) (EVInt i1) (EVInt i2) = EVInt (i1 * i2)
  (*) _ _ = error "Multiplication of non-integer values"
  abs (EVInt i) = EVInt (abs i)
  abs _ = error "Abs of non-integer value"
  signum (EVInt i) = EVInt (signum i)
  signum _ = error "Signum of non-integer value"
  fromInteger = EVInt
  negate (EVInt i) = EVInt (negate i)
  negate _ = error "Negation of non-integer value"

getBoolValue :: Value -> Bool
getBoolValue (EVBool b) = b
getBoolValue _ = error "Not a boolean value"

instance Eq Value where
  (==) (EVInt i1) (EVInt i2) = i1 == i2
  (==) (EVBool b1) (EVBool b2) = b1 == b2
  (==) _ _ = error "Comparison of non-integer values"

getStringValue :: Value -> String
getStringValue (EVString s) = s
getStringValue _ = error "Not a string value"

instance Show Value where
  show (EVInt i) = show i
  show (EVBool b) = show b
  show (EVString s) = s
  show EVVoid = "void"
  show (EVFun {}) = "function"

type EvaluatorResult a = StateT EvalState (ExceptT String IO) a

class Evaluator a where
  eval :: a -> EvaluatorResult Value

type Loc = Integer

data Store = Store {
  locToVal :: Map Loc Value,
  nextLoc :: Loc
}

instance Show Store where
  show (Store locToVal nextLoc) = show locToVal

emptyStore :: Store
emptyStore = Store {
  locToVal = Map.empty,
  nextLoc = 0
}

addToStore :: Value -> Store -> Store
addToStore v s = Store {
  locToVal = Map.insert (nextLoc s) v (locToVal s),
  nextLoc = nextLoc s + 1
}

changeValue :: Ident -> Value -> EvalState -> EvalState
changeValue ident v evalState =
  let loc = identToLoc (env evalState) Map.! ident
  in evalState {
    store = Store {
      locToVal = Map.insert loc v (locToVal (store evalState)),
      nextLoc = nextLoc (store evalState)
    }
  }

getValue :: Ident -> EvalState -> Value
getValue ident evalState = locToVal (store evalState) Map.! (identToLoc (env evalState) Map.! ident)

getArgIdents :: Expr -> Ident
getArgIdents (EVar _ ident) = ident
getArgIdents _ = Ident ""

getArgTypes :: [Arg] -> [Arg]
getArgTypes [] = []
getArgTypes ((IArg pos t ident):args) = (IArg pos t ident):(getArgTypes args)
getArgTypes ((VarArg pos t ident):args) = (VarArg pos t ident):(getArgTypes args)

getItemIdent :: Item -> Ident
getItemIdent (NoInit _ ident) = ident

data Env = Env {
  identToLoc :: Map Ident Loc
}

instance Show Env where
  show (Env env) = show env

addToEnv :: Ident -> Loc -> Env -> Env
addToEnv ident loc env = Env {
  identToLoc = Map.insert ident loc (identToLoc env)
}

emptyEnv :: Env
emptyEnv = Env {
  identToLoc = Map.empty
}

data EvalState = EvalState {
  env :: Env,
  store :: Store
}

instance Show EvalState where
  show (EvalState env store) = "Pers env: " ++ show env ++ "and store: " ++ show store

showPosition :: BNFC'Position -> String
showPosition (Just (l, c)) = "line " ++ show l ++ ", column " ++ show c

emptyEvalState :: EvalState
emptyEvalState = EvalState {
  env = emptyEnv,
  store = emptyStore
}

findLocByIdent :: Ident -> EvalState -> Loc
findLocByIdent ident evalState = identToLoc (env evalState) Map.! ident

addToEvalState :: Ident -> Value -> EvalState -> EvalState
addToEvalState ident v evalState = evalState {
  store = addToStore v $ store evalState,
  env = addToEnv ident (nextLoc (store evalState)) (env evalState)
}

addToEvalStateWithCustomLoc :: Ident -> Value -> Loc -> EvalState -> EvalState
addToEvalStateWithCustomLoc ident v loc evalState = evalState {
  store = store evalState,
  env = addToEnv ident loc (env evalState)
}

getFromEvalState :: Ident -> EvalState -> Value
getFromEvalState ident evalState = locToVal (store evalState) Map.! (identToLoc (env evalState) Map.! ident)

setEnvInEvalState :: Env -> EvalState -> EvalState
setEnvInEvalState env evalState = evalState {
  env = env
}

setStoreInEvalState :: Store -> EvalState -> EvalState
setStoreInEvalState store evalState = evalState {
  store = store
}

returnIdent :: Ident
returnIdent = Ident "return"

checkReturnAndProceed :: EvaluatorResult Value -> EvaluatorResult Value
checkReturnAndProceed action = do
  envBefore <- get
  if hasReturnValue envBefore
    then return EVVoid
    else action

hasReturnValue :: EvalState -> Bool
hasReturnValue evalState = returnIdent `Map.member` identToLoc (env evalState)

getReturnValue :: EvalState -> Value
getReturnValue evalState = getFromEvalState returnIdent evalState

setReturnValue :: Value -> EvalState -> EvalState
setReturnValue val evalState = addToEvalState returnIdent val evalState

runEvaluator :: Program -> IO (Either String Value)
runEvaluator p = runExceptT $ evalStateT (eval p) emptyEvalState

instance Evaluator Program where
  eval (IProgram pos topDefs) = do
    mapM_ eval topDefs
    mainRes <- eval $ EApp pos (Ident "main") []
    evalState <- get
    liftIO $ print mainRes
    return EVVoid

instance Evaluator TopDef where
  eval (FnDef _ retType ident args block) = do
    evalState <- get
    let loc = nextLoc (store evalState)
    put evalState {
      env = addToEnv ident loc (env evalState),
      store = addToStore (EVFun retType (addToEnv ident loc (env evalState)) ident args block) (store evalState)
    }
    return EVVoid

  eval (VarDef _ _ ident expr) = do
    evalState <- get
    v <- eval expr
    let loc = nextLoc (store evalState)
    put evalState {
      env = addToEnv ident loc (env evalState),
      store = addToStore v (store evalState)
    }
    return EVVoid

instance Evaluator Block where
  eval (IBlock _ stmts) = do
    mapM_ eval stmts
    return EVVoid

instance Evaluator Expr where
  eval (ELitInt _ i) = return $ EVInt i
  eval (ELitTrue _) = return $ EVBool True
  eval (ELitFalse _) = return $ EVBool False
  eval (EString _ s) = return $ EVString s
  eval (Neg pos expr) = do
    v <- eval expr
    case v of
      EVInt i -> return $ EVInt (-i)
      _ -> throwError $ "Negation of non-integer value at " ++ showPosition pos
  eval (Not pos expr) = do
    v <- eval expr
    case v of
      EVBool b -> return $ EVBool (not b)
      _ -> throwError $ "Negation of non-boolean value at " ++ showPosition pos
  eval (EMul pos expr1 op expr2) = do
    v1 <- eval expr1
    v2 <- eval expr2
    case (v1, v2) of
      (EVInt i1, EVInt i2) -> case op of
        Times _ -> return $ EVInt (i1 * i2)
        Div _ -> if i2 == 0 then throwError $ "Division by zero at " ++ showPosition pos ++ " illegal" else return $ EVInt (i1 `div` i2)
        Mod _ -> return $ EVInt (i1 `mod` i2)
      _ -> throwError $ "Multiplication of non-integer values at " ++ showPosition pos
  eval (EAdd pos expr1 op expr2) = do
    v1 <- eval expr1
    v2 <- eval expr2
    case (v1, v2) of
      (EVInt i1, EVInt i2) -> case op of
        Plus _ -> return $ EVInt (i1 + i2)
        Minus _ -> return $ EVInt (i1 - i2)
      _ -> throwError $ "Addition of non-integer values at " ++ showPosition pos
  eval (ERel pos expr1 op expr2) = do
    v1 <- eval expr1
    v2 <- eval expr2
    case (v1, v2) of
      (EVInt i1, EVInt i2) -> case op of
        LTH _ -> return $ EVBool (i1 < i2)
        LE _ -> return $ EVBool (i1 <= i2)
        GTH _ -> return $ EVBool (i1 > i2)
        GE _ -> return $ EVBool (i1 >= i2)
        EQU _ -> return $ EVBool (i1 == i2)
        NE _ -> return $ EVBool (i1 /= i2)
      (EVBool b1, EVBool b2) -> case op of
        EQU _ -> return $ EVBool (b1 == b2)
        NE _ -> return $ EVBool (b1 /= b2)
        _ -> throwError $ "Comparison illegal at " ++ showPosition pos ++ " . Legal comparators for booleans are == and !="
      (EVString s1, EVString s2) -> case op of
        EQU _ -> return $ EVBool (s1 == s2)
        NE _ -> return $ EVBool (s1 /= s2)
        _ -> throwError $ "Comparison illegal at " ++ showPosition pos ++ " . Legal comparators for strings are = and !="
      _ -> throwError $ "Comparison of non-comparable values at " ++ showPosition pos
  eval (EAnd pos expr1 expr2) = do
    v1 <- eval expr1
    case v1 of
      EVBool b1 -> if b1 then eval expr2 else return $ EVBool False
      _ -> throwError $ "And of non-boolean values at " ++ showPosition pos
  eval (EOr pos expr1 expr2) = do
    v1 <- eval expr1
    case v1 of
      EVBool b1 -> if b1 then return $ EVBool True else eval expr2
      _ -> throwError $ "Or of non-boolean values at " ++ showPosition pos
  eval (EVar _ ident) = do
    evalState <- get
    return $ getFromEvalState ident evalState
  eval (EApp pos ident exprs) =
    case ident of
      Ident "printInt" -> do
        v <- eval $ head exprs
        liftIO $ print $ getIntegerValue v
        return EVVoid
      Ident "printString" -> do
        v <- eval $ head exprs
        liftIO $ print $ getStringValue v
        return EVVoid
      Ident "printBool" -> do
        v <- eval $ head exprs
        liftIO $ print $ getBoolValue v
        return EVVoid
      Ident name -> do
        evalState <- get -- evalState to stan z momentu wywołania funkcji
        let loc = identToLoc (env evalState) Map.! ident
        let EVFun retType funEnv _ args block = locToVal (store evalState) Map.! loc
        let evalStateFromInit = setEnvInEvalState funEnv evalState -- evalState z momentu inicjalizacji funkcji

        argValues <- mapM eval exprs
        put $ setStoreInEvalState (store evalState) evalStateFromInit
        mapM_ (\(argIdent, value, argType) -> case argType of
          IArg _ t ident -> modify $ addToEvalState ident value
          VarArg _ t ident -> do
            modify $ addToEvalStateWithCustomLoc ident value (findLocByIdent argIdent evalState)
          ) (zip3 (map getArgIdents exprs) argValues (getArgTypes args)) -- dodajemy argumenty do stanu z momentu inicjalizacji funkcji
        evalStateFromInitWithArgs <- get
        result <- eval block -- z takim stanem (opisanym nieco wyżej) wykonujemy blok funkcji
        evalStateFromBlock <- get -- stan po wykonaniu bloku funkcji
        if not (hasReturnValue evalStateFromBlock) then
          throwError $ "Function " ++ show ident ++ " at position " ++ showPosition pos ++ "does not have a return statement in all branches"
          else do
            let returnVal = getReturnValue evalStateFromBlock -- odzyskujemy wartość zwracaną przez blok funkcji
            put $ addToEvalState returnIdent returnVal $ evalState { -- do stanu z momentu wywołania funkcji dodajemy wartość zwracaną przez blok funkcji
              env = (env evalState) { identToLoc = Map.insert ident loc (identToLoc (env evalState)) },
              store = store evalStateFromBlock -- store niezmiennie idzie "do przodu"
            }
            newEvalState <- get -- obecny stan to stan z bloku wywołującego funkcję zmodifikowany o wartość zwracaną przez funkcję oraz nadpisanie zmiennych
            if ident /= returnIdent then do
              put $ newEvalState { env = (env newEvalState) { identToLoc = Map.delete (Ident "return") (identToLoc (env newEvalState)) } }
              return returnVal
            else do
              return returnVal

instance Evaluator Stmt where
  eval (Empty _) = return EVVoid

  eval (BStmt _ block) = checkReturnAndProceed $ do
    oldEvalState <- get
    let oldEnv = env oldEvalState
    eval block
    newEvalState <- get
    if hasReturnValue newEvalState
      then do
        put $ addToEvalState returnIdent (getReturnValue newEvalState) oldEvalState
        return EVVoid
      else do
        put newEvalState { env = oldEnv }
        return EVVoid

  eval (Decl _ t items) = checkReturnAndProceed $ do
    evalState <- get
    let v = getValueDefaultInit t
    put evalState {
      store = addToStore v (store evalState),
      env = foldl (\env item -> addToEnv (getItemIdent item) (nextLoc (store evalState)) env) (env evalState) items
    }
    return EVVoid

  eval (FVInit _ topDef) = checkReturnAndProceed $ eval topDef

  eval (Ass _ ident expr) = checkReturnAndProceed $ do
    exprEval <- eval expr
    modify $ changeValue ident exprEval
    return EVVoid

  eval (Incr _ ident) = checkReturnAndProceed $ do
    evalState <- get
    modify $ changeValue ident (getValue ident evalState + 1)
    return EVVoid

  eval (Decr _ ident) = checkReturnAndProceed $ do
    evalState <- get
    modify $ changeValue ident (getValue ident evalState - 1)
    return EVVoid

  eval (Ret _ expr) = checkReturnAndProceed $ do
    evalExpr <- eval expr
    evalState <- get
    put $ setReturnValue evalExpr evalState
    return evalExpr

  eval (VRet _) = checkReturnAndProceed $ do
    evalState <- get
    put $ setReturnValue EVVoid evalState
    return EVVoid

  eval (Cond _ expr block) = checkReturnAndProceed $ do
    evalExpr <- eval expr
    oldEvalState <- get
    let oldEnv = env oldEvalState
    let evalBool = getBoolValue evalExpr
    if evalBool then eval block else return EVVoid
    newEvalState <- get
    if hasReturnValue newEvalState
      then do
        put $ addToEvalState returnIdent (getReturnValue newEvalState) oldEvalState
        return EVVoid
      else do
        put newEvalState { env = oldEnv }
        return EVVoid

  eval (CondElse _ expr block1 block2) = checkReturnAndProceed $ do
    evalExpr <- eval expr
    oldEvalState <- get
    let oldEnv = env oldEvalState
    let evalBool = getBoolValue evalExpr
    if evalBool then eval block1 else eval block2
    newEvalState <- get
    if hasReturnValue newEvalState
      then do
        put $ addToEvalState returnIdent (getReturnValue newEvalState) oldEvalState
        return EVVoid
      else do
        put newEvalState { env = oldEnv }
        return EVVoid

  eval (While pos expr block) = checkReturnAndProceed $ do
    evalExpr <- eval expr
    oldEvalState <- get
    let oldEnv = env oldEvalState
    let evalBool = getBoolValue evalExpr
    if evalBool then do
      eval block
      eval $ While pos expr block
    else return EVVoid
    newEvalState <- get
    if hasReturnValue newEvalState
      then do
        put $ addToEvalState returnIdent (getReturnValue newEvalState) oldEvalState
        return EVVoid
      else do
        put newEvalState { env = oldEnv }
        return EVVoid

  eval (SExp _ expr) = checkReturnAndProceed $ eval expr