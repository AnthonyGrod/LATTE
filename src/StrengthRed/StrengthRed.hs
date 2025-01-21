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


strengthReducaAllWhiles :: CompilerM ()
strengthReducaAllWhiles = do
  whileBlocks <- gets whileBlocks
  mapM_ strengthReduceWhile (Map.keys whileBlocks)

strengthReduceWhile :: Label -> CompilerM ()
strengthReduceWhile labelBefore = do
  whileBlocks <- gets whileBlocks
  let whileBlockLabels = fromJust $ Map.lookup labelBefore whileBlocks

  basicBlocks <- gets basicBlocks
  let allInstrs = concatMap (bbInstructions . (\label -> fromJust $ Map.lookup label basicBlocks)) whileBlockLabels

  let allIVOrgTriples = allInductionVarOriginTriples allInstrs allInstrs
  if null allIVOrgTriples then return ()
  else do
    mapM_ (reduceStrengthInBB labelBefore allIVOrgTriples) whileBlockLabels

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

reduceStrengthInBB ::  Label -> [(LLVMValue, LLVMValue, LLVMValue)] -> Label -> CompilerM ()
reduceStrengthInBB labelBefore allIVOrgTriples whileBBToReduce = do
  givenBB <- gets $ fromJust . Map.lookup whileBBToReduce . basicBlocks
  let bbInstrs = bbInstructions givenBB
  newInstrs <- mapM (\instr -> if doesInstrQualifyForStrengthReduction instr allIVOrgTriples then do
                                            let instrLHS = getRHSReg instr
                                            let instrLHSOrigin = fromJust $ lookupTriple instrLHS allIVOrgTriples
                                            uncurry (reduceStrengthInstr labelBefore instr) instrLHSOrigin
                                            else return instr) bbInstrs
  replaceBasicBlockInstructions whileBBToReduce newInstrs where
    lookupTriple :: LLVMValue -> [(LLVMValue, LLVMValue, LLVMValue)] -> Maybe (LLVMValue, LLVMValue)
    lookupTriple iv [] = Nothing
    lookupTriple iv ((iv', org, constVal):triples) = if iv == iv' then Just (org, constVal) else lookupTriple iv triples



reduceStrengthInstr :: Label -> Instr -> LLVMValue -> LLVMValue -> CompilerM Instr
reduceStrengthInstr labelBefore mulInstr origin constVal = do
  -- add instruction to the beginning of beforeLabel and then replace the mulInstr with addInstr and return mulInstr.
  let (IBinOp lhs rhs1 rhs2 BMul) = mulInstr
  let (EVInt c) = constVal
  let (EVInt constVal, EVReg iv) = case (rhs1, rhs2) of
        (EVInt i, EVReg r) -> (EVInt i, EVReg r)
        (EVReg r, EVInt i) -> (EVInt i, EVReg r)
  forBeforeReg <- getNextRegisterAndIncrement
  forAfterReg <- getNextRegisterAndIncrement
  let mulInitInstr = IBinOp (EVReg forBeforeReg) (EVInt constVal) origin BMul
  let afterLabelInstr = IPhi (EVReg forAfterReg) TVInt (EVReg forBeforeReg, labelBefore - 1) (lhs, labelBefore + 2)
  let addInstr = IBinOp lhs (EVReg forAfterReg) (EVInt (constVal * c)) BAdd
  -- insert mulInitInstr to the absolute beginning of labelBefore
  basicBlocks <- gets basicBlocks
  let instrs = mulInitInstr : bbInstructions (fromJust $ Map.lookup labelBefore basicBlocks)
  replaceBasicBlockInstructions labelBefore instrs
  insertAfterFirstLabelOccurence labelBefore afterLabelInstr
  return addInstr where
    insertAfterFirstLabelOccurence :: Label -> Instr -> CompilerM ()
    insertAfterFirstLabelOccurence label instr = do
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
doesInstrQualifyForStrengthReduction :: Instr -> [(LLVMValue, LLVMValue, LLVMValue)] -> Bool
doesInstrQualifyForStrengthReduction instr inductionVarAndOriginPairs = case instr of
  IBinOp lhs rhs1 rhs2 op -> case op of
    BMul -> case (rhs1, rhs2) of
      (EVInt _, EVReg r) -> any (\(iv, _, _) -> iv == EVReg r) inductionVarAndOriginPairs
      (EVReg r, EVInt _) -> any (\(iv, _, _) -> iv == EVReg r) inductionVarAndOriginPairs
      _ -> False
    _ -> False
  _ -> False

allInductionVarOriginTriples :: [Instr] -> [Instr] -> [(LLVMValue, LLVMValue, LLVMValue)]
allInductionVarOriginTriples instrs allInstrs = allInductionVarOriginTriples' instrs allInstrs [] where
  allInductionVarOriginTriples' :: [Instr] -> [Instr] -> [(LLVMValue, LLVMValue, LLVMValue)] -> [(LLVMValue, LLVMValue, LLVMValue)]
  allInductionVarOriginTriples' [] allInstrs acc = acc
  allInductionVarOriginTriples' (instr:instrs) allInstrs acc = case instr of
    IBinOp lhs rhs1 rhs2 op ->
      if not (checkIfValidInductionVar lhs rhs1 rhs2 allInstrs acc) then
        allInductionVarOriginTriples' instrs allInstrs (removeInductionVarPair lhs acc)
      else
        case op of
        BAdd -> case (rhs1, rhs2) of
          (EVReg r, EVInt i) -> allInductionVarOriginTriples' instrs allInstrs ((lhs, getOrigin (EVReg r) allInstrs, EVInt i):acc)
          (EVInt i, EVReg r) -> allInductionVarOriginTriples' instrs allInstrs ((lhs, getOrigin (EVReg r) allInstrs, EVInt i):acc)
          _ -> allInductionVarOriginTriples' instrs allInstrs acc
        BSub -> case (rhs1, rhs2) of
          (EVReg r, EVInt i) -> allInductionVarOriginTriples' instrs allInstrs ((lhs, getOrigin (EVReg r) allInstrs, EVInt (-i)):acc)
          (EVInt i, EVReg r) -> allInductionVarOriginTriples' instrs allInstrs ((lhs, getOrigin (EVReg r) allInstrs, EVInt (-i)):acc)
          _ -> allInductionVarOriginTriples' instrs allInstrs acc
        _ -> allInductionVarOriginTriples' instrs allInstrs acc
    _ -> allInductionVarOriginTriples' instrs allInstrs acc

getOrigin :: LLVMValue -> [Instr] -> LLVMValue
getOrigin reg [] = error "getOrigin: origin not found"
getOrigin reg (instr:instrs) =
  case instr of
  (IPhi lhsReg typ (val1, label1) (val2, label2)) -> if lhsReg == reg then val1 else getOrigin reg instrs
  _ -> getOrigin reg instrs

removeInductionVarPair :: LLVMValue -> [(LLVMValue, LLVMValue, LLVMValue)] -> [(LLVMValue, LLVMValue, LLVMValue)]
removeInductionVarPair iv = filter (\(v1,v2,_) -> v1 /= iv && v2 /= iv)

checkIfValidInductionVar :: LLVMValue -> LLVMValue -> LLVMValue -> [Instr] -> [(LLVMValue, LLVMValue, LLVMValue)] -> Bool
checkIfValidInductionVar iv rhs1 rhs2 instrs alreadyExisting = case instrs of
  [] -> False
  (instr:instrs) -> case instr of
    IAss lhs rhs -> if lhs == iv then not (any (\(org2, org, _) ->
      org == lhs || org == rhs1 || org == rhs2 ||
      org2 == lhs || org2 == rhs1 || org2 == rhs2) alreadyExisting) else checkIfValidInductionVar iv rhs1 rhs2 instrs alreadyExisting
    IBinOp lhs rhs1 rhs2 op -> if lhs == iv then not (any (\(org2, org, _) ->
      org == lhs || org == rhs1 || org == rhs2 ||
      org2 == lhs || org2 == rhs1 || org2 == rhs2) alreadyExisting) else checkIfValidInductionVar iv rhs1 rhs2 instrs alreadyExisting
    _ -> checkIfValidInductionVar iv rhs1 rhs2 instrs alreadyExisting

