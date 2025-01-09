module RunCompiler (compileWithArgs, compile) where

import Prelude
import Frontend.Frontend
import Parser.Par
import Parser.Abs
import Compiler.Backend


compileWithArgs :: String -> IO ()
compileWithArgs file = readFile file >>= compile file

extractProgram :: Program -> [TopDef' BNFC'Position]
extractProgram (Program _ p) = p

compile :: String -> String -> IO ()
compile fileNameWithPath input = case pProgram (myLexer input) of
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
        putStrLn "OK"
        result <- runCompiler program fileNameWithPath
        print ""
          