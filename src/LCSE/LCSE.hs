{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module LCSE.LCSE where

import Aux
import State
import Parser.Abs
import Data.Map (Map)
import qualified Data.Map as Map

optimizeBlockLCSE :: BasicBlock -> BasicBlock
optimizeBlockLCSE block = block { bbInstructions = optimizeInstructionsLCSE (bbInstructions block) [] }

-- will take instr -: (lhs, rhs) and if for some instruction in instrs -: x 
-- there will be rhs_x == rhs then replace ALL lhs_x occurences with lhs
optimizeInstructionsLCSE :: [Instr] -> [Instr] -> [Instr]
optimizeInstructionsLCSE [] acc = acc
optimizeInstructionsLCSE (instr:instrs) acc = do
  if isAssignableOp instr
    then do
      let instrsAfterStep = optimizeLCSEStep instr instrs [] -- should return all instructions after instr with potentially removed and replaced stuff
      optimizeInstructionsLCSE instrsAfterStep (acc ++ [instr])
    else do
      optimizeInstructionsLCSE instrs (acc ++ [instr])

-- reg - register that will be put in the replaced places
-- reg_x - register that will be replaced
-- instrs - list of instructions
-- returns list of instructions with all occurences of reg_x replaced with reg
replaceAllRegisters :: LLVMValue -> LLVMValue -> [Instr] -> [Instr]
replaceAllRegisters reg reg_x = map (replaceRegInInstrRHS reg reg_x)

-- will take instr -: (lhs, rhs) and if for some instruction in instrs -: x 
-- there will be rhs_x == rhs then replace ALL lhs_x occurences with lhs
optimizeLCSEStep :: Instr -> [Instr] -> [Instr] -> [Instr]
optimizeLCSEStep focus [] acc = acc
optimizeLCSEStep focus (head:rest) acc =
  if areInstrsTheSameType focus head
    then 
      case focus of
      IAss lhs rhs ->
        let headRHS = getAssRHS head
            headLHS = getLHSReg head
        in if headRHS == rhs
             then
               let instrsAfterReplace = replaceAllRegisters lhs headLHS rest
               in optimizeLCSEStep focus instrsAfterReplace acc
             else optimizeLCSEStep focus rest (acc ++ [head])
      IBinOp lhs rhs1 rhs2 op ->
        let headRHS = getBinOpRHS head
            headLHS = getLHSReg head
        in if headRHS == [rhs1, rhs2]
             then
               let instrsAfterReplace = replaceAllRegisters lhs headLHS rest
               in optimizeLCSEStep focus instrsAfterReplace acc
             else optimizeLCSEStep focus rest (acc ++ [head])
      IRelOp lhs typ rhs1 rhs2 op ->
        let headRHS = getRelOpRHS head
            headLHS = getLHSReg head
        in if headRHS == [rhs1, rhs2]
             then
               let instrsAfterReplace = replaceAllRegisters lhs headLHS rest
               in optimizeLCSEStep focus instrsAfterReplace acc
             else optimizeLCSEStep focus rest (acc ++ [head])
    else optimizeLCSEStep focus rest (acc ++ [head])