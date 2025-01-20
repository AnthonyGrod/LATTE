{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module State where

import Aux
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
  , identToValueAndType :: Map Ident ValueAndType
  , identToFunSig    :: Map Ident LLVMValue
  , allInstructions  :: [Instr]
  , returnValue      :: ValueAndType
  , doNotReturn      :: Bool
  , globalStringMap  :: Map StringNum String
  , nextFreeStringNum :: StringNum
  , blocksOrder      :: [Label]
  , whileBlocks      :: Map Label [Instr] -- starting while label and its instructions
  }

initialState :: CompileState
initialState = CompileState
  { nextFreeLabelNum = 0
  , nextFreeRegNum = 1
  , basicBlocks = Map.empty
  , currBasicBlockLabel = 0
  , identToValueAndType = Map.empty
  , identToFunSig = Map.empty
  , allInstructions = []
  , returnValue = dummyReturnValueAndType
  , doNotReturn = False
  , globalStringMap = Map.fromList [(1, "")] -- for empty string
  , nextFreeStringNum = 2
  , blocksOrder = []
  , whileBlocks = Map.empty
  }

setIdentToValueAndTypeToEmpty :: CompileState -> CompileState
setIdentToValueAndTypeToEmpty state = state { identToValueAndType = Map.empty }

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
  return $ returnValue state /= dummyReturnValueAndType

setRetValueToDummy :: CompilerM ()
setRetValueToDummy = modify $ \st -> st { returnValue = dummyReturnValueAndType }

setRetValue :: ValueAndType -> CompilerM ()
setRetValue regAndType = modify $ \st -> st { returnValue = regAndType }

getRetValue :: CompilerM ValueAndType
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

insertStringToGlobalStringMap :: StringNum -> String -> CompilerM ()
insertStringToGlobalStringMap num str = modify $ \st ->
  st { globalStringMap = Map.insert num str (globalStringMap st) }

getStringFromGlobalStringMap :: StringNum -> CompilerM String
getStringFromGlobalStringMap num = do
  state <- get
  case Map.lookup num (globalStringMap state) of
    Just str -> return str
    Nothing -> error $ "String with number " ++ show num ++ " not found"

getNextRegisterAndIncrement :: CompilerM Register
getNextRegisterAndIncrement = do
  state <- get
  let currentRegisterNum = nextFreeRegNum state
  put state { nextFreeRegNum = currentRegisterNum + 1 }
  return currentRegisterNum

getCurrentBasicBlockLabel :: CompilerM Label
getCurrentBasicBlockLabel = gets currBasicBlockLabel

insertIdentValueAndType :: Ident -> LLVMValue -> LLVMType -> CompilerM ()
insertIdentValueAndType ident reg typ = do
  modify $ \st -> st
    { identToValueAndType = Map.insert ident (reg, typ) (identToValueAndType st) }

lookupIdentValueAndType :: Ident -> CompilerM ValueAndType
lookupIdentValueAndType ident = do
  state <- get
  case Map.lookup ident (identToValueAndType state) of
    Just regAndType -> return regAndType
    Nothing -> error $ "Variable " ++ extractIdent ident ++ " not found"

insertIdentFunSigs :: [(Ident, LLVMValue)] -> CompilerM ()
insertIdentFunSigs identsAndVals = modify $ \st ->
  st { identToFunSig = foldl (\m (i, v) -> Map.insert i v m) (identToFunSig st) identsAndVals }

addGenLLVM :: Instr -> CompilerM ()
addGenLLVM instr = do
  state <- get
  let currLabel = currBasicBlockLabel state
  let bbMap     = basicBlocks state
  let currBB    = bbMap Map.! currLabel
  let newBB     = currBB { bbInstructions = bbInstructions currBB ++ [instr] }
  put state
    { basicBlocks     = Map.insert currLabel newBB bbMap
    , allInstructions = allInstructions state ++ [instr]
    }

getGenLLVM :: CompilerM [Instr]
getGenLLVM = gets allInstructions

setcurrBasicBlockLabel :: Label -> CompilerM ()
setcurrBasicBlockLabel label = modify $ \st -> st { 
  currBasicBlockLabel = label, blocksOrder = blocksOrder st ++ [label]
  }

getBlocksOrder :: CompilerM [Label]
getBlocksOrder = gets blocksOrder

getBasicBlock :: Label -> CompilerM BasicBlock
getBasicBlock label = do
  state <- get
  return $ basicBlocks state Map.! label

getBasicBlockGenLLVM :: Label -> CompilerM [Instr]
getBasicBlockGenLLVM label = do
  bb <- getBasicBlock label
  return $ bbInstructions bb

getAllBasicBlocksGenLLVM :: CompilerM [Instr]
getAllBasicBlocksGenLLVM = do
  gets (concatMap bbInstructions . Map.elems . basicBlocks)

getAllBasicBlocks :: CompilerM (Map Label BasicBlock)
getAllBasicBlocks = gets basicBlocks

insertBasicBlock :: BasicBlock -> CompilerM ()
insertBasicBlock bb = do
  state <- get
  put state { basicBlocks = Map.insert (bbLabel bb) bb (basicBlocks state) }

insertEmptyBasicBlock :: Label -> CompilerM ()
insertEmptyBasicBlock label = insertBasicBlock $ emptyBasicBlock label
