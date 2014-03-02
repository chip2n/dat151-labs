module Interpreter where

import AbsGrammar

import Prelude hiding (lookup)
import qualified Data.Map as Map
import Control.Monad.State

data EvalMode = CallByName | CallByValue

interpret :: Program -> EvalMode -> IO Integer
interpret p mode = return $ evalState (execProgram p mode) emptyEnv

execProgram :: Program -> EvalMode -> State Env Integer
execProgram (PDefs defs) mode = do
    buildFunMap defs
    VClosure e _ <- lookup (Ident "main")
    --VInt i <- evalExp e
    --return i
    v <- evalExp e mode
    error $ "Final value: " ++ show v

buildFunMap :: [Def] -> State Env ()
buildFunMap = mapM_ (\(DFun i args e) -> update i (createEmptyClosure e args))

execDef :: Def -> EvalMode -> State Env Value
execDef (DFun _ _ e) mode = evalExp e mode

evalExp :: Exp -> EvalMode -> State Env Value
evalExp e mode = case e of
    EId i -> do
        value <- lookup i
        return value
    EInt i -> return $ VInt i
    EApp e1 e2 -> do
        case mode of
            CallByName -> do
              v1 <- evalExp e1 mode
              v2 <- evalExp e2 mode
              case v1 of
                  (VClosure (EAbs args e') env) -> do
                      if length args > 1
                          then do let firstArg = (\(Arg i) -> i) (head args)
                                  let env' = Map.insert firstArg v2 env
                                  return (VClosure (EAbs (tail args) e') env')
                          else do let firstArg = (\(Arg i) -> i) (head args)
                                  let env' = Map.insert firstArg v2 env
                                  originalState <- get
                                  modify (\_ -> env' `Map.union` originalState)
                                  v <- evalExp (EAbs (tail args) e') mode
                                  modify (\_ -> originalState)
                                  return v
            CallByValue -> do
                v2@(VInt i2) <- evalExp e2 mode
                evalExp e1 mode
    EAdd e1 e2 -> do
        (VInt i1) <- evalExp e1 mode
        (VInt i2) <- evalExp e2 mode
        return (VInt (i1 + i2))
    ESub e1 e2 -> undefined
    ELt  e1 e2 -> undefined
    EIf  e1 e2 e3 -> undefined
    EAbs args e -> do
        if args == []
            then evalExp e mode
            else do s <- get
                    return $ VClosure e s
        --mapM_ (\(Arg (Ident i)) -> update i )

lambdaArgs :: Exp -> Int
lambdaArgs (EAbs args _) = length args

createEmptyClosure :: Exp -> [Arg] -> Value
createEmptyClosure e args = VClosure (EAbs args e) emptyEnv

type Env = Map.Map Ident Value

data Value = VInt Integer
           | VClosure Exp Env deriving (Show)

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
