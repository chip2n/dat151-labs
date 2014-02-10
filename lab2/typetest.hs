import System.Directory
import AbsCPP
import LexCPP
import ParCPP
import ErrM

import TypeChecker
import Interpreter
import Data.List

main :: IO ()
main = do
    goodFiles <- getDirectoryContents "/home/chip/git/dat151-labs/lab2/testsuite/good"
    badFiles <- getDirectoryContents "/home/chip/git/dat151-labs/lab2/testsuite/bad"

    let good'' = delete ".." . delete "." $ goodFiles
    let good' = filter (\x -> isSuffixOf ".cc" x) good''
    let good = filter (\x -> not . isPrefixOf "." $ x) good'

    let bad'' = delete ".." . delete "." $ badFiles
    let bad' = filter (\x -> isSuffixOf ".cc" x) bad''
    let bad = filter (\x -> not . isPrefixOf "." $ x) bad'

    testAll good
    putStrLn "-----------------"
    testAll' bad




testAll :: [FilePath] -> IO ()
testAll xs = mapM_ (\x -> test $ "/home/chip/git/dat151-labs/lab2/testsuite/good/" ++ x) xs

testAll' :: [FilePath] -> IO ()
testAll' xs = mapM_ (\x -> test $ "/home/chip/git/dat151-labs/lab2/testsuite/bad/" ++ x) xs

test :: FilePath -> IO ()
test x = do
    s <- readFile x
    case pProgram (myLexer s) of
        Bad err -> putStrLn $ "Failed parsing of: " ++ x
        Ok tree -> case typecheck tree of
                        Bad err -> putStrLn $ "Failed: " ++ x
                        Ok tree -> putStrLn $ "Success: " ++ x
    return ()
