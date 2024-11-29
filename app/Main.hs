module Main where

import Prelude 
import Interpreter
import System.Environment

main = do
  args <- getArgs
  case args of
    [f] -> interpretWithFile f
    [] -> interpretStdin

interpretStdin = do
  getContents >>= interpret
