{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module StrengthRed.StrengthRed where

import Aux
import State
import Parser.Abs
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Data.Maybe (fromJust)
import qualified Control.Monad
import Debug.Trace (trace)


strengthReducaAllWhiles :: CompilerM ()
strengthReducaAllWhiles = do
  whileBlocks <- gets whileBlocks
  liftIO $ print whileBlocks
  mapM_ strengthReduceWhile (Map.keys whileBlocks)

strengthReduceWhile :: Label -> CompilerM ()
strengthReduceWhile labelBefore = do
  -- I will have to add the strength init to origin as the first instruction in labelBefore BB
  -- (just before the br)
  whileBlocks <- gets whileBlocks
  let whileBlockLabels = fromJust $ Map.lookup labelBefore whileBlocks

  basicBlocks <- gets basicBlocks
  let allInstrs = concatMap (bbInstructions . (\label -> fromJust $ Map.lookup label basicBlocks)) whileBlockLabels

  let allIVOrgPairs = allInductionVarOriginPairs allInstrs allInstrs
  if null allIVOrgPairs then return ()
  else do
    liftIO $ print "-------------allIVOrgPairs------------: "
    liftIO $ print allIVOrgPairs
    liftIO $ print "-------------------------: "

    mapM_ (reduceStrengthInBB labelBefore allIVOrgPairs) whileBlockLabels
    -- Then I will need to add a phi node to labelBefore.
    -- And at the end reduce actual mul to add with const, phi result.

-- we assume that one of the vals on the RHS is a constant and the other is an induction variable - we need to return the IV
getRHSReg :: Instr -> LLVMValue
getRHSReg instr = case instr of
  IBinOp lhs rhs1 rhs2 op -> case op of
    BMul -> case (rhs1, rhs2) of
      (EVInt _, EVReg r) -> EVReg r
      (EVReg r, EVInt _) -> EVReg r
      _ -> error "getRHSReg: not a mul instr"
    _ -> error "getRHSReg: not a mul instr"
  _ -> error "getRHSReg: not a mul instr"

reduceStrengthInBB ::  Label -> [(LLVMValue, LLVMValue)] -> Label -> CompilerM ()
reduceStrengthInBB labelBefore allIVOrgPairs whileBBToReduce = do
  givenBB <- gets $ fromJust . Map.lookup whileBBToReduce . basicBlocks
  let bbInstrs = bbInstructions givenBB
  newInstrs <- mapM (\instr -> if doesInstrQualifyForStrengthReduction instr allIVOrgPairs then do
                                            liftIO $ print "instr that does qualify lhs----------: "
                                            liftIO $ print $ instr
                                            liftIO $ print $ getRHSReg instr
                                            let instrLHS = getRHSReg instr
                                            liftIO $ print "-------------------: "
                                            liftIO $ print $ lookup instrLHS allIVOrgPairs
                                            liftIO $ print "xx-------------------xx: "
                                            liftIO $ print allIVOrgPairs
                                            let instrLHSOrigin = fromJust $ lookup instrLHS allIVOrgPairs
                                            instrRep <- reduceStrengthInstr labelBefore instr instrLHSOrigin
                                            liftIO $ print "instrRep: "
                                            liftIO $ print instrRep
                                            return instrRep
                                            else return instr) bbInstrs
  replaceBasicBlockInstructions whileBBToReduce newInstrs



-- labelBody = labelBefore + 2
reduceStrengthInstr :: Label -> Instr -> LLVMValue -> CompilerM Instr
reduceStrengthInstr labelBefore mulInstr origin = do
  -- add instruction to the beginning of beforeLabel and then replace the mulInstr with addInstr and return mulInstr.
  -- unpack mulInstr
  liftIO $ putStrLn "reduceStrengthInstr: "
  liftIO $ print mulInstr
  let (IBinOp lhs rhs1 rhs2 BMul) = mulInstr
  let (EVReg reg) = lhs
  let (EVInt constVal, EVReg iv) = case (rhs1, rhs2) of
        (EVInt i, EVReg r) -> (EVInt i, EVReg r)
        (EVReg r, EVInt i) -> (EVInt i, EVReg r)
  forBeforeReg <- getNextRegisterAndIncrement
  forAfterReg <- getNextRegisterAndIncrement
  let mulInitInstr = IBinOp (EVReg forBeforeReg) (EVInt constVal) origin BMul
  let afterLabelInstr = IPhi (EVReg forAfterReg) TVInt (EVReg forBeforeReg, labelBefore - 1) (lhs, labelBefore + 2)
  let addInstr = IBinOp lhs (EVReg forAfterReg) (EVInt constVal) BAdd
  liftIO $ print "addInstr: "
  liftIO $ print addInstr
  -- insert mulInitInstr to the absolute beginning of labelBefore
  basicBlocks <- gets basicBlocks
  let instrs = mulInitInstr : bbInstructions (fromJust $ Map.lookup labelBefore basicBlocks)
  replaceBasicBlockInstructions labelBefore instrs
  insertAfterFirstLabelOccurence labelBefore afterLabelInstr
  return addInstr where
    insertAfterFirstLabelOccurence :: Label -> Instr -> CompilerM ()
    insertAfterFirstLabelOccurence label instr = do
      liftIO $ print "insertAfterFirstLabelOccurence: "
      liftIO $ print instr
      basicBlocks <- gets basicBlocks
      let givenBB = fromJust $ Map.lookup label basicBlocks
      let bbInstrs = bbInstructions givenBB
      let (before, after) = break (\instr -> case instr of ILabel _ -> True; _ -> False) bbInstrs
      let newBBInstrs = case after of
                          [] -> before ++ [instr]
                          _  -> before ++ [head after, instr] ++ tail after
      replaceBasicBlockInstructions label newBBInstrs

-- In order for a instruction to qualify for strength reduction, it needs to be a multiplication where one
-- expr is a constant number value and the other expr is an induction variable
doesInstrQualifyForStrengthReduction :: Instr -> [(LLVMValue, LLVMValue)] -> Bool
doesInstrQualifyForStrengthReduction instr inductionVarAndOriginPairs =
  trace ("doesInstrQualifyForStrengthReduction: " ++ show instr) $ case instr of
  IBinOp lhs rhs1 rhs2 op -> case op of
    BMul -> case (rhs1, rhs2) of
      (EVInt _, EVReg r) -> any (\(iv, _) -> iv == EVReg r) inductionVarAndOriginPairs
      (EVReg r, EVInt _) -> any (\(iv, _) -> iv == EVReg r) inductionVarAndOriginPairs
      _ -> False
    _ -> False
  _ -> False

allInductionVarOriginPairs :: [Instr] -> [Instr] -> [(LLVMValue, LLVMValue)]
allInductionVarOriginPairs instrs allInstrs = allInductionVarOriginPairs' instrs allInstrs [] where
  allInductionVarOriginPairs' :: [Instr] -> [Instr] -> [(LLVMValue, LLVMValue)] -> [(LLVMValue, LLVMValue)]
  allInductionVarOriginPairs' [] allInstrs acc = acc
  allInductionVarOriginPairs' (instr:instrs) allInstrs acc = case instr of
    IBinOp lhs rhs1 rhs2 op -> 
      if not (checkIfValidInductionVar lhs allInstrs) then allInductionVarOriginPairs' instrs allInstrs acc
      else
        case op of
        BAdd -> case (rhs1, rhs2) of
          (EVReg r, EVInt i) -> allInductionVarOriginPairs' instrs allInstrs ((lhs, getOrigin (EVReg r) allInstrs):acc)
          (EVInt i, EVReg r) -> allInductionVarOriginPairs' instrs allInstrs ((lhs, getOrigin (EVReg r) allInstrs):acc)
          _ -> allInductionVarOriginPairs' instrs allInstrs acc
        BSub -> case (rhs1, rhs2) of
          (EVReg r, EVInt i) -> allInductionVarOriginPairs' instrs allInstrs ((lhs, getOrigin (EVReg r) allInstrs):acc)
          (EVInt i, EVReg r) -> allInductionVarOriginPairs' instrs allInstrs ((lhs, getOrigin (EVReg r) allInstrs):acc)
          _ -> allInductionVarOriginPairs' instrs allInstrs acc
        _ -> allInductionVarOriginPairs' instrs allInstrs acc
    _ -> allInductionVarOriginPairs' instrs allInstrs acc

-- Search for origin of reg: find %reg = phi [%origin, %somelabel]
getOrigin :: LLVMValue -> [Instr] -> LLVMValue
getOrigin reg [] = error "getOrigin: origin not found"
getOrigin reg (instr:instrs) =
  case instr of
  (IPhi lhsReg typ (val1, label1) (val2, label2)) -> if lhsReg == reg then val1 else getOrigin reg instrs
  _ -> getOrigin reg instrs

-- check if only one assignment to induction var. TODO: don't accept IV in if, while
checkIfValidInductionVar :: LLVMValue -> [Instr] -> Bool
checkIfValidInductionVar iv instrs = length (filter (isAssignToIV iv) instrs) == 1

isAssignToIV :: LLVMValue -> Instr -> Bool
isAssignToIV iv instr = case instr of
  IAss lhs rhs -> lhs == iv
  IBinOp lhs rhs1 rhs2 op -> lhs == iv
  _ -> False





-- TODO: I might need this.
  -- removeAllDuplicates acc
  -- where
  --   removeAllDuplicates :: (Eq a) => [a] -> [a]
  --   removeAllDuplicates xs = filter (\x -> count x xs == 1) xs

  --   count :: (Eq a) => a -> [a] -> Int
  --   count x = length . filter (== x)