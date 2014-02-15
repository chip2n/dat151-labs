module CodeGenerator where

import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State

import AbsCPP

compileStm :: Stm -> State Env ()
compileStm s = undefined

compileExp :: Exp -> State Env ()
compileExp e = undefined


emit :: Instruction -> State Env ()
emit i = modify (\s -> s {code = i : code s})




data Env = Env { vars :: [Map.Map Id Int]
               , maxvar :: Int
               , code :: [Instruction]
               } deriving (Show)

type Instruction = Integer


lookupVar :: Id -> State Env Int
lookupVar x = do
    s <- get
    -- then look up the first occurence of x in (vars s)
    undefined
