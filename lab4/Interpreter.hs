module Interpreter where

import AbsGrammar

import Prelude hiding (lookup)
import qualified Data.Map as Map

data EvalMode = CallByName | CallByValue deriving (Eq)

interpret :: Program -> EvalMode -> Integer
interpret = execProgram

execProgram :: Program -> EvalMode -> Integer
execProgram (PDefs defs) mode = i
  where VClosure mainExp _ = lookup (Ident "main") env
        env = buildFunMap defs
        v = evalExp env mainExp mode
        i = case v of
                VInt i' -> i'
                VClosure (EInt i') _ -> i'

buildFunMap :: [Def] -> Env
buildFunMap = foldl (\env (DFun i args e) -> updateFun env i (createEmptyClosure e args)) emptyEnv

execDef :: Env -> Def -> EvalMode -> Value
execDef env (DFun _ _ e) = evalExp env e

evalExp :: Env -> Exp -> EvalMode -> Value
evalExp env e mode = case e of
    EId i -> let VClosure e' env' = lookup i env
             in evalExp (Env (functions env) (variables env')) e' mode
    EInt _ -> VClosure e emptyEnv -- maybe variables env?
    EApp e1 e2 -> let VClosure (EAbs x e') env' = evalExp env e1 mode
                      u = case mode of
                              CallByName -> VClosure e2 (Env Map.empty $ variables env)
                              CallByValue -> evalExp env e2 mode
                      newEnv = updateVar (Env (functions env) (variables env')) x u
                  in evalExp newEnv e' mode
    EAdd e1 e2 -> let u = value $ evalExp env e1 mode
                      v = value $ evalExp env e2 mode
                  in VClosure (EInt (u + v)) emptyEnv
    ESub e1 e2 -> let u = value $ evalExp env e1 mode
                      v = value $ evalExp env e2 mode
                  in VClosure (EInt (u - v)) emptyEnv
    ELt e1 e2 -> let u = value $ evalExp env e1 mode
                     v = value $ evalExp env e2 mode
                 in VInt (if u < v then 1 else 0)
    EIf c a b -> let u = evalExp env c mode
                 in if u == VInt 1
                     then evalExp env a mode
                     else evalExp env b mode
    EAbs _ _ -> VClosure e (Env Map.empty $ variables env)


value :: Value -> Integer
value (VInt i) = i
value (VClosure (EInt i) _) = i
value _ = error "Not possible to get integer value."

createEmptyClosure :: Exp -> [Ident] -> Value
createEmptyClosure e args = VClosure (absConv e args) emptyEnv

absConv :: Exp -> [Ident] -> Exp
absConv e [] = e
absConv e args = absConv (EAbs i e) (init args)
  where i = last args

data Env = Env {
    functions :: Map.Map Ident Value,
    variables :: Map.Map Ident Value
} deriving (Show, Eq)

data Value = VInt Integer
           | VClosure Exp Env deriving (Show, Eq)

emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty

lookup :: Ident -> Env -> Value
lookup i@(Ident s) env =
    case lookupVar i env of
        Nothing -> case lookupFun i env of
                       Nothing -> error $ "Variable " ++ s ++ " not found. Env: " ++ show env
                       Just v  -> v
        Just v  -> v

lookupFun :: Ident -> Env -> Maybe Value
lookupFun i env = Map.lookup i (functions env)

lookupVar :: Ident -> Env -> Maybe Value
lookupVar i env = Map.lookup i (variables env)

updateFun :: Env -> Ident -> Value -> Env
updateFun env i v = env { functions = Map.insert i v (functions env) }

updateVar :: Env -> Ident -> Value -> Env
updateVar env i v = env { variables = Map.insert i v (variables env) }
