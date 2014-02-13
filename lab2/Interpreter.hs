module Interpreter where

import AbsCPP
import PrintCPP
import ErrM

import Control.Monad
import qualified Data.Map as M

interpret :: Program -> IO ()
interpret (PDefs defs) = putStrLn "no interpreter yet"

execStms :: Env -> [Stm] -> IO Env
execStms env [] = return env
execStms env (s:stms) = do env' <- execStm env s
                           execStms env' stms

execStm :: Env -> Stm -> IO Env
execStm env s = do
    case s of
        SExp e           -> evalExp env e >> return env
        SDecls t xs      -> undefined
        SInit t i e      -> do
            let env' = addVar env i
            (value, env'') <- evalExp env' e
            return $ setVar env'' i value
        SReturn e        -> undefined
        SWhile e s1      -> undefined
        SBlock xs        -> execStms (enterScope env) xs
        SIfElse e s1 s2  -> undefined

evalExp :: Env -> Exp -> IO (Value,Env)
evalExp env e = 
    case e of
        ETrue         -> undefined
        EFalse        -> undefined
        EInt i        -> undefined
        EDouble d     -> undefined
        EId x         -> return (lookupVar env x, env)
        EApp i es     -> undefined
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

emptyEnv :: Env
emptyEnv = undefined

addVar :: Env -> Id -> Env
addVar = undefined

setVar :: Env -> Id -> Value -> Env
setVar = undefined

lookupVar :: Env -> Id -> Value
lookupVar = undefined

enterScope :: Env -> Env
enterScope = undefined

leaveScope :: Env -> Env
leaveScope = undefined
