module Interpreter where

import AbsGrammar

import Prelude hiding (lookup)
import qualified Data.Map as Map
import Control.Monad.State

interpret :: Program -> IO Integer
interpret p = return $ evalState (execProgram p) emptyEnv

execProgram :: Program -> State Env Integer
execProgram (PDefs defs)= do
    buildFunMap defs
    VClosure e _ <- lookup (Ident "main")
    VInt i <- evalExp e
    return i

buildFunMap :: [Def] -> State Env ()
buildFunMap = mapM_ (\(DFun i args e) -> update i (createEmptyClosure e args))

execDef :: Def -> State Env Value
execDef (DFun _ _ e) = evalExp e

evalExp :: Exp -> State Env Value
evalExp e = case e of
    EId i -> lookup i
    EInt i -> return $ VInt i
    EApp e1 e2 -> undefined
    EAdd e1 e2 -> do
        (VInt i1) <- evalExp e1
        (VInt i2) <- evalExp e2
        return (VInt (i1 + i2))
    ESub e1 e2 -> undefined
    ELt  e1 e2 -> undefined
    EIf  e1 e2 e3 -> undefined
    EAbs args e -> do
        mapM_ (\(Arg (Ident i)) -> update i )

createEmptyClosure :: Exp -> [Arg] -> Value
createEmptyClosure e args = VClosure (EAbs args e) emptyEnv

type Env = Map.Map Ident Value

data Value = VInt Integer
           | VClosure Exp Env

emptyEnv :: Env
emptyEnv = Map.empty

lookup :: Ident -> State Env Value
lookup i@(Ident s)= do
    env <- get
    case Map.lookup i env of
        Nothing -> fail $ "Variable " ++ s ++ " not found."
        Just v  -> return v

update :: Ident -> Value -> State Env ()
update i v = modify (\env -> Map.insert i v env)
