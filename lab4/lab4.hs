import System.Environment (getArgs)
import System.Exit (exitFailure)

import ParGrammar
import ErrM
import PrintGrammar

import Interpreter

check :: String -> IO () 
check s = case pProgram (myLexer s) of
            Bad err  -> do putStrLn "SYNTAX ERROR"
                           putStrLn err
                           exitFailure 
            Ok  tree -> do putStrLn $ printTree tree
                           i <- interpret tree
                           putStrLn $ "Result: " ++ show i
                           --Ok t -> putStrLn $ show t

main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> readFile file >>= check
            _      -> do putStrLn "Usage: lab3 <SourceFile>"
                         exitFailure
