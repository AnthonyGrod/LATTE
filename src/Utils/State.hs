{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Utils.State where

import Utils.Types
import Utils.Aux
import Parser.Abs
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity (Identity)


data CompileState = CompileState
  { nextFreeLabelNum :: Int
  , nextFreeRegNum   :: Register
  , basicBlocks      :: Map Label BasicBlock
  , currBasicBlockLabel :: Label
  , identToRegisterAndType :: Map Ident RegisterAndType
  , identToFunSig    :: Map Ident LLVMValue
  , allInstructions  :: [Instr]
  , returnValue      :: RegisterAndType
  , doNotReturn      :: Bool
  , globalStringMap  :: Map StringNum String
  , nextFreeStringNum :: StringNum
  }

initialState :: CompileState
initialState = CompileState
  { nextFreeLabelNum = 0
  , nextFreeRegNum = 1
  , basicBlocks = Map.empty
  , currBasicBlockLabel = -1
  , identToRegisterAndType = Map.empty
  , identToFunSig = Map.empty
  , allInstructions = []
  , returnValue = dummyReturnRegisterAndType
  , doNotReturn = False
  , globalStringMap = Map.fromList [(1, "")] -- for empty string
  , nextFreeStringNum = 2
  }

setIdentToRegisterAndTypeToEmpty :: CompileState -> CompileState
setIdentToRegisterAndTypeToEmpty state = state { identToRegisterAndType = Map.empty }

type CompilerM a = StateT CompileState IO a

data BasicBlock = BasicBlock
  { bbLabel        :: Label
  , bbInstructions :: [Instr]
  }

emptyBasicBlock :: Label -> BasicBlock
emptyBasicBlock label = BasicBlock
  { bbLabel = label
  , bbInstructions = []
  }

checkIfReturnValueNotDummy :: CompilerM Bool
checkIfReturnValueNotDummy = do
  state <- get
  return $ returnValue state /= dummyReturnRegisterAndType

addRetValueGenLLVM :: CompilerM ()
addRetValueGenLLVM = do
  state <- get
  let (reg, regType) = returnValue state
  if (reg, regType) == dummyReturnRegisterAndType
    then return ()
    else addGenLLVM $ IFunRet (EVReg reg) regType

setRetValueToDummy :: CompilerM ()
setRetValueToDummy = modify $ \st -> st { returnValue = dummyReturnRegisterAndType }

setRetValue :: RegisterAndType -> CompilerM ()
setRetValue regAndType = modify $ \st -> st { returnValue = regAndType }

getRetValue :: CompilerM RegisterAndType
getRetValue = gets returnValue

setDoNotReturn :: Bool -> CompilerM ()
setDoNotReturn b = modify $ \st -> st { doNotReturn = b }

getDoNotReturn :: CompilerM Bool
getDoNotReturn = gets doNotReturn

getNextLabelAndIncrement :: CompilerM Int
getNextLabelAndIncrement = do
  state <- get
  let currentLabel = nextFreeLabelNum state
  put state { nextFreeLabelNum = currentLabel + 1 }
  return currentLabel

getNextStringNumAndIncrement :: CompilerM StringNum
getNextStringNumAndIncrement = do
  state <- get
  let currentStringNum = nextFreeStringNum state
  put state { nextFreeStringNum = currentStringNum + 1 }
  return currentStringNum

getStringFromGlobalStringMap :: StringNum -> CompilerM String
getStringFromGlobalStringMap num = do
  state <- get
  case Map.lookup num (globalStringMap state) of
    Just s -> return s
    Nothing -> error $ "String with number " ++ show num ++ " not found"

insertStringToGlobalStringMap :: StringNum -> String -> CompilerM ()
insertStringToGlobalStringMap num str = modify $ \st -> st { globalStringMap = Map.insert num str (globalStringMap st) }

getNextRegisterAndIncrement :: CompilerM Register
getNextRegisterAndIncrement = do
  state <- get
  let currentRegisterNum = nextFreeRegNum state
  put state { nextFreeRegNum = currentRegisterNum + 1 }
  return currentRegisterNum

getCurrentBasicBlockLabel :: CompilerM Label
getCurrentBasicBlockLabel = gets currBasicBlockLabel

insertIdentRegisterAndType :: Ident -> Register -> LLVMType -> CompilerM ()
insertIdentRegisterAndType ident reg typ = do
  modify $ \st -> st { identToRegisterAndType = Map.insert ident (reg, typ) (identToRegisterAndType st) }

lookupIdentRegisterAndType :: Ident -> CompilerM RegisterAndType
lookupIdentRegisterAndType ident = do
  state <- get
  case Map.lookup ident (identToRegisterAndType state) of
    Just regAndType -> return regAndType
    Nothing -> error $ "Variable " ++ extractIdent ident ++ " not found"

lookupIdentRegisterAndTypeInOldState :: CompileState -> Ident -> CompilerM RegisterAndType
lookupIdentRegisterAndTypeInOldState state ident =
  case Map.lookup ident (identToRegisterAndType state) of
    Just regAndType -> return regAndType
    Nothing -> error $ "Variable " ++ extractIdent ident ++ " not found"

insertIdentFunSig :: Ident -> LLVMValue -> CompilerM ()
insertIdentFunSig ident val = modify $ \st ->
  st { identToFunSig = Map.insert ident val (identToFunSig st) }

insertIdentFunSigs :: [(Ident, LLVMValue)] -> CompilerM ()
insertIdentFunSigs identsAndVals = modify $ \st ->
  st { identToFunSig = foldl (\m (ident, val) -> Map.insert ident val m) (identToFunSig st) identsAndVals }

addGenLLVM :: Instr -> CompilerM ()
addGenLLVM instr = do
  state <- get
  let currLabel = currBasicBlockLabel state
  let bb = basicBlocks state
  let currBB = bb Map.! currLabel
  let newBB = currBB { bbInstructions = bbInstructions currBB ++ [instr] }
  put state { basicBlocks = Map.insert currLabel newBB bb, allInstructions = allInstructions state ++ [instr] }

getGenLLVM :: CompilerM [Instr]
getGenLLVM = gets allInstructions

setcurrBasicBlockLabel :: Label -> CompilerM ()
setcurrBasicBlockLabel label = modify $ \st -> st { currBasicBlockLabel = label }

getBasicBlock :: Label -> CompilerM BasicBlock
getBasicBlock label = do
  state <- get
  return $ basicBlocks state Map.! label

getAllBasicBlocksGenLLVM :: CompilerM [Instr]
getAllBasicBlocksGenLLVM = do
  gets (concatMap bbInstructions . Map.elems . basicBlocks)

getBasicBlockGenLLVM :: Label -> CompilerM [Instr]
getBasicBlockGenLLVM label = do
  bb <- getBasicBlock label
  return $ bbInstructions bb

insertBasicBlock :: BasicBlock -> CompilerM ()
insertBasicBlock bb = do
  state <- get
  put state { basicBlocks = Map.insert (bbLabel bb) bb (basicBlocks state) }

insertEmptyBasicBlock :: Label -> CompilerM ()
insertEmptyBasicBlock label = insertBasicBlock $ emptyBasicBlock label

insertInstrToBasicBlock :: Label -> Instr -> CompilerM ()
insertInstrToBasicBlock label instr = do
  bb <- getBasicBlock label
  let newBB = bb { bbInstructions = bbInstructions bb ++ [instr] }
  insertBasicBlock newBB

insertInstrToCurrBasicBlock :: Instr -> CompilerM ()
insertInstrToCurrBasicBlock instr = do
  state <- get
  let currLabel = currBasicBlockLabel state
  let bb = basicBlocks state
  let currBB = bb Map.! currLabel
  let newBB = currBB { bbInstructions = bbInstructions currBB ++ [instr] }
  put state { basicBlocks = Map.insert currLabel newBB bb }
