module RunCompiler (interpretWithStdin, interpretWithFile, interpret) where

import Prelude
import Frontend.Frontend
import Parser.Par
import Parser.Abs
import Compiler.Backend

interpretWithStdin :: String -> IO ()
interpretWithStdin = interpret

interpretWithFile :: String -> IO ()
interpretWithFile file = readFile file >>= interpret

extractProgram :: Program -> [TopDef' BNFC'Position]
extractProgram (Program _ p) = p

interpret :: String -> IO ()
interpret input = case pProgram (myLexer input) of
  Left err -> 
    putStrLn "ERROR" >>
    putStrLn err
  Right program -> do
    -- putStrLn $ show program
    result <- typecheck program
    case result of
      Left err -> do
        putStrLn "ERROR"
        putStrLn err
      Right tree -> do
        result <- runCompiler program
        print ""
          