module Interpreter where

import AbsCPP
import PrintCPP
import ErrM

import Control.Monad
import qualified Data.Map as M

interpret :: Program -> IO ()
interpret (PDefs defs) = do
    let env = foldl setFun emptyEnv defs
    evalExp env (EApp (Id "main") [])
    return ()

execDef :: Env -> Def -> IO (Value, Env)
execDef env (DFun t i args stms) = undefined
    

execStms :: Env -> [Stm] -> IO Env
execStms env [] = return env
execStms env (s:stms) = do env' <- execStm env s
                           execStms env' stms

execStm :: Env -> Stm -> IO Env
execStm env s =
    case s of
        SExp e           -> evalExp env e >>= return . snd
        SDecls _ xs      -> return $ foldl (addVar) env xs
        SInit t i e      -> do
            let env' = addVar env i
            (value, env'') <- evalExp env' e
            return $ setVar env'' i value
        SReturn e        -> return env
        SWhile e s1      -> do
            (val, env') <- evalExp env e
            if val == VBool True
                then do 
                    env'' <- execStm env' s1
                    execStm env'' s1
                else return env' 
        SBlock xs        -> do
            env' <- execStms (enterScope env) xs
            return (leaveScope env')
        SIfElse e s1 s2  -> do
            (val, env') <- evalExp env e
            if val == VBool True
                then execStm env' s1
                else execStm env' s2

evalExp :: Env -> Exp -> IO (Value,Env)
evalExp env e = 
    case e of
        ETrue         -> return (VBool True, env)
        EFalse        -> return (VBool False, env)
        EInt i        -> return (VInt i, env)
        EDouble d     -> return (VDouble d, env)
        EId x         -> return (lookupVar env x, env)
        EApp i es     -> do
            env' <- foldM evalExp env es
            let (DFun _ _ args stms) = lookupFun env' i
            let args' = map (\(ADecl at it) -> it) args

            return undefined
             
        EPIncr e      -> undefined
        EPDecr e      -> undefined
        EIncr e       -> undefined
        EDecr e       -> undefined
        ETimes e1 e2  -> undefined
        EDiv e1 e2    -> undefined
        EPlus e1 e2   -> do
            (v1, env')  <- evalExp env e1
            (v2, env'') <- evalExp env' e2
            case (v1,v2) of
                (VInt i1, VInt i2)       -> return (VInt (i1+i2), env'')
                (VDouble d1, VDouble d2) -> return (VDouble (d1+d2), env'')
        ELt e1 e2     -> undefined
        EGt e1 e2     -> undefined
        ELtEq e1 e2   -> undefined
        EGtEq e1 e2   -> undefined
        EEq e1 e2     -> undefined
        ENEq e1 e2    -> undefined
        EAnd e1 e2    -> undefined
        EOr e1 e2     -> undefined
        EAss e1 e2    -> undefined


type Env = [[(Id, Value)]]
data Value = VInt Integer
           | VDouble Double
           | VBool Bool
           | VVoid
           | VUndef
           deriving (Eq, Show)

emptyEnv :: Env
emptyEnv = undefined

addVar :: Env -> Id -> Env
addVar = undefined

setVar :: Env -> Id -> Value -> Env
setVar = undefined

setFun :: Env -> Def -> Env
setFun = undefined

lookupVar :: Env -> Id -> Value
lookupVar = undefined

lookupFun :: Env -> Id -> Def
lookupFun = undefined

enterScope :: Env -> Env
enterScope = undefined

leaveScope :: Env -> Env
leaveScope = undefined
