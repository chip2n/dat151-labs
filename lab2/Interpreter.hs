module Interpreter where

import AbsCPP
import PrintCPP
import ErrM

import Control.Monad
import qualified Data.Map as M

interpret :: Program -> IO ()
interpret (PDefs defs) = putStrLn "no interpreter yet"
