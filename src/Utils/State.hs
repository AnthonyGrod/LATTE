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

-- insertVarDeclaredInCurrBlock :: Ident -> CompilerM ()
-- insertVarDeclaredInCurrBlock ident = do
--   state <- get
--   let currLabel = currBasicBlockLabel state
--   let bb = basicBlocks state
--   let currBB = bb Map.! currLabel
--   let newBB = currBB { varsDeclaredInCurrBlock = varsDeclaredInCurrBlock currBB ++ [ident] }
--   put state { basicBlocks = Map.insert currLabel newBB bb }

-- insertVarChangedFromPredBlock :: Ident -> RegisterAndType -> CompilerM ()
-- insertVarChangedFromPredBlock ident regAndType = do
--   state <- get
--   let currLabel = currBasicBlockLabel state
--   let bb = basicBlocks state
--   let currBB = bb Map.! currLabel
--   let newBB = currBB { varsChangedFromPredBlock = Map.insert ident (regAndType, currLabel) (varsChangedFromPredBlock currBB) }
--   put state { basicBlocks = Map.insert currLabel newBB bb }

-- insertVarChangedFromPredBlockToBlock :: Label -> Ident -> RegisterAndType -> Label -> CompilerM ()
-- insertVarChangedFromPredBlockToBlock blockLabel ident regAndType label = do
--   bb <- getBasicBlock blockLabel
--   let newBB = bb { varsChangedFromPredBlock = Map.insert ident (regAndType, label) (varsChangedFromPredBlock bb) }
--   insertBasicBlock newBB


-- isIdentInVarsDeclaredInCurrBlock :: Ident -> CompilerM Bool
-- isIdentInVarsDeclaredInCurrBlock ident = do
--   state <- get
--   let currLabel = currBasicBlockLabel state
--   let bb = basicBlocks state
--   let currBB = bb Map.! currLabel
--   return $ elem ident (varsDeclaredInCurrBlock currBB)

