module Interpreter (interpretWithStdin, interpretWithFile, interpret) where

import Prelude
import Typechecker.Typechecker
import Parser.Par
import Parser.Abs

interpretWithStdin :: String -> IO ()
interpretWithStdin = interpret

interpretWithFile :: String -> IO ()
interpretWithFile file = readFile file >>= interpret

extractProgram :: Program -> [TopDef' BNFC'Position]
extractProgram (IProgram _ p) = p

interpret :: String -> IO ()
interpret input = case pProgram (myLexer input) of
  Left err -> 
    putStrLn "Parse failed" >>
    putStrLn err
  Right program -> do
    result <- typecheck program
    case result of
      Left err -> putStrLn err
      Right _ -> putStrLn "GOOD"
          