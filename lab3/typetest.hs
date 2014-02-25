import System.Directory
import Control.Concurrent.MVar
import Control.Concurrent
import System.IO
import System.Environment
import System.Exit (exitFailure)
import System.Process (runInteractiveCommand, system)
import AbsCPP
import LexCPP
import ParCPP
import ErrM

import TypeChecker
import CodeGenerator
import Data.List

main :: IO ()
main = do
    [path] <- getArgs
    goodFiles <- getDirectoryContents path
    let good = delete ".." . delete "." $ goodFiles
    let good' = filter (\x -> isSuffixOf ".cc" x) good
    testAll $ sort good'

testAll :: [FilePath] -> IO ()
testAll xs = mapM_ (\x -> test $ "/home/chip/git/dat151-labs/lab3/testsuite/good/" ++ x) xs

test :: FilePath -> IO ()
test x = do
    s <- readFile x
    case pProgram (myLexer s) of
        Bad err -> putStrLn $ "Failed parsing of: " ++ x
        Ok tree -> case typecheck tree of
                        Bad err -> putStrLn $ "Failed: " ++ x
                        Ok tree -> execute x $ compile "test" tree
    return ()

type Command = String
type Input = String
type Error = String
type Output = String

runCommand :: Command -> Input -> IO (Output, Error)
runCommand cmd inp = do
    (inpHandle, outHandle, errHandle, pid) <- runInteractiveCommand cmd
    outVar <- newEmptyMVar
    errVar <- newEmptyMVar
    forkIO $ do
        hPutStr inpHandle inp
        hClose inpHandle
    forkIO $ do
        s <- hGetContents outHandle
        putMVar outVar s
    forkIO $ do
        s <- hGetContents errHandle
        putMVar errVar s
    o <- takeMVar outVar
    e <- takeMVar errVar
    return (o, e)

execute :: FilePath -> String -> IO ()
execute x s = do
    putStrLn "--------------------------------------------------------------------"
    putStrLn $ "File: " ++ x
    putStrLn ""
    writeFile "lab3.j" s
    system "jasmin lab3.j > /dev/null"
    input <- readFile $ x ++ ".input"
    expectedOutput <- readFile $ x ++ ".output"
    (o,e) <- runCommand "java -cp . test" input

    if e == ""
        then return ()
        else putStrLn $ "Error:\n" ++ e

    if o == expectedOutput
        then putStrLn "Such parse, many correct, very code - wow"
        else do
            putStrLn $ "Input:\n" ++ input
            putStrLn $ "Output:\n" ++ o
            putStrLn $ "Expected output:\n" ++ expectedOutput

    return ()

createFile :: String -> IO ()
createFile code = do
    system "java -cp . test"
    return ()
