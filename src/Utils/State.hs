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
  { nextFreeLabelNum :: Int      -- next free label number
  , nextFreeRegNum   :: Register
  , basicBlocks      :: Map Label BasicBlock
  , currBasicBlockLabel :: Label -- we need to know in which basic block we are currently so we can emit instructions to it
  , identToRegisterAndType :: Map Ident RegisterAndType
  , identToFunSig    :: Map Ident LLVMValue
  , allInstructions  :: [Instr]
  , returnValue      :: RegisterAndType
  , doNotReturn      :: Bool
  -- , bbPredecessor    :: Label
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
  -- , bbPredecessor = -1
  }

-- write a function that sets identToRegisterAndType to empty
setIdentToRegisterAndTypeToEmpty :: CompileState -> CompileState
setIdentToRegisterAndTypeToEmpty state = state { identToRegisterAndType = Map.empty }

type CompilerM a = StateT CompileState IO a

data BasicBlock = BasicBlock
  { bbLabel        :: Label
  , bbInstructions :: [Instr]
  -- , varsDeclaredInCurrBlock :: [Ident]
  -- , varsChangedFromPredBlock :: Map Ident (RegisterAndType, Label)
  }

emptyBasicBlock :: Label -> BasicBlock
emptyBasicBlock label = BasicBlock
  { bbLabel = label
  , bbInstructions = []
  -- , varsDeclaredInCurrBlock = []
  -- , varsChangedFromPredBlock = Map.empty
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

-- getPredecessorLabel :: CompilerM Label
-- getPredecessorLabel = gets bbPredecessor

-- setPredecessorLabel :: Label -> CompilerM ()
-- setPredecessorLabel label = modify $ \st -> st { bbPredecessor = label }

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

-- insertEmptyBasicBlockWithCopiedVars :: Label -> CompilerM ()
-- insertEmptyBasicBlockWithCopiedVars label = do
--   state <- get
--   let currLabel = currBasicBlockLabel state
--   let bb = basicBlocks state
--   let currBB = bb Map.! currLabel
--   let newBB = (emptyBasicBlock label) { varsDeclaredInCurrBlock = varsDeclaredInCurrBlock currBB
--                                       , varsChangedFromPredBlock = varsChangedFromPredBlock currBB }
--   insertBasicBlock newBB

-- insertEmptyBasicBlockWithCopiedVarsFromBlock :: Label -> Label -> CompilerM ()
-- insertEmptyBasicBlockWithCopiedVarsFromBlock label blockLabel = do
--   bb <- getBasicBlock blockLabel
--   let newBB = (emptyBasicBlock label) { varsDeclaredInCurrBlock = varsDeclaredInCurrBlock bb
--                                       , varsChangedFromPredBlock = varsChangedFromPredBlock bb }
--   insertBasicBlock newBB

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

-- copyRegisterToNextFree :: Register -> LLVMType -> CompilerM Register
-- copyRegisterToNextFree reg typ = do
--   case typ of
--     TVInt -> do
--       zeroReg <- getNextRegisterAndIncrement
--       addGenLLVM $ IAss (EVReg zeroReg) (EVInt 0)
--       newReg <- getNextRegisterAndIncrement
--       addGenLLVM $ IBinOp (EVReg newReg) (EVReg zeroReg) (EVReg reg) BAdd
--       return newReg
--     TVBool -> do
--       falseReg <- getNextRegisterAndIncrement
--       addGenLLVM $ IBinOp (EVReg falseReg) (EVBool False) (EVBool False) BOr
--       newReg <- getNextRegisterAndIncrement
--       addGenLLVM $ IBinOp (EVReg newReg) (EVReg falseReg) (EVReg reg) BOr
--       return newReg
--     TVVoid -> return reg
--     _ -> error "Cannot copy register of this type" -- TODO: String manipulation
