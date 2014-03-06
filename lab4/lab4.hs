import System.Environment (getArgs)
import System.Exit (exitFailure)

import ParGrammar
import ErrM
import PrintGrammar

import Interpreter

type Flag = String

check :: Flag -> String-> IO () 
check flag s = case pProgram (myLexer s) of
            Bad err  -> do putStrLn "SYNTAX ERROR"
                           putStrLn err
                           exitFailure 
            Ok  tree -> do putStrLn $ printTree tree
                           let i = case flag of
                                    "-n" -> interpret tree CallByName 
                                    "-v" -> interpret tree CallByValue
                           putStrLn $ "Result: " ++ show i
                           --Ok t -> putStrLn $ show t

main :: IO ()
main = do args <- getArgs
          case args of
            (flag:file:_) -> readFile file >>= check flag
            _      -> do putStrLn "Usage: lab3 (-n|-v) <SourceFile>"
                         exitFailure
