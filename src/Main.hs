module Main where

import Prelude
import RunCompiler
import System.Environment

main = do
  args <- getArgs
  let firstArg = head args
  compileWithArgs firstArg

