module Interpreter where

import AbsCPP
import PrintCPP
import ErrM

import Control.Monad
import qualified Data.Map as Map

interpret :: Program -> IO ()
interpret (PDefs defs) = do
    let env = foldl setFun emptyEnv defs
    evalExp env (EApp (Id "main") [])
    return ()

execDef :: Env -> Def -> IO (Value, Env)
execDef env (DFun t i args ((SReturn e):ss)) = do
    evalExp env e
execDef env (DFun t i args (s:ss)) = do
    env' <- execStm env s
    execDef env' (DFun t i args ss)
execDef env (DFun t i args ([])) = return (VVoid, env)
    

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
                    execStm env'' s
                else return env' 
        SBlock xs        -> do
            env' <- execStms (enterScope env) xs
            return (env')
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

        -- BUILT-IN FUNCTIONS --
        EApp (Id "printInt") e' -> do
            (VInt i, env') <- evalExp env (head e')
            putStrLn $ show i
            return (VVoid, env')

        EApp (Id "printDouble") e' -> do
            (VDouble d, env') <- evalExp env (head e')
            putStrLn $ show d
            return (VVoid, env')

        EApp (Id "readInt") _ -> do
            i <- getLine
            return (VInt (read i), env)

        EApp (Id "readDouble") _ -> do
            d <- getLine
            return (VDouble (read d), env)

        EApp i es     -> do
            (vals, env') <- foldM foldEval ([], env) es
            let def@(DFun _ _ args stms) = lookupFun env' i
            let argsNames = map (\(ADecl at it) -> it) args

            let env'' = enterScope env'
            let a = zip argsNames vals

            let env''' = foldl (\e (arg,val) -> setVar (addVar e arg) arg val) env'' a
            (resVal, resEnv) <- execDef env''' def
            return (resVal, leaveScope resEnv)
            
        EPIncr (EId i) -> do
            (val, env') <- evalExp env (EId i)
            let newVal = case val of
                            VInt i -> VInt $ i + 1
                            VDouble d -> VDouble $ d + 1
                            _   -> error "sdfgdg"

            let env'' = setVar env' i (newVal)
            return (val, env'')
        EPDecr (EId i) -> do
            (val, env') <- evalExp env (EId i)
            let newVal = case val of
                            VInt i -> VInt $ i - 1
                            VDouble d -> VDouble $ d - 1
                            _   -> error "sdfgdg"

            let env'' = setVar env' i (newVal)
            return (val, env'')
        EIncr (EId i) -> do
            (val, env') <- evalExp env (EId i)
            let newVal = case val of
                            VInt i -> VInt $ i + 1
                            VDouble d -> VDouble $ d + 1
                            _   -> error "sdfgdg"

            let env'' = setVar env' i (newVal)
            return (newVal, env'')
        EDecr (EId i) -> do
            (val, env') <- evalExp env (EId i)
            let newVal = case val of
                            VInt i -> VInt $ i - 1
                            VDouble d -> VDouble $ d - 1
                            _   -> error "sdfgdg"

            let env'' = setVar env' i (newVal)
            return (newVal, env'')
        ETimes e1 e2   -> do
            (v1, env')  <- evalExp env e1
            (v2, env'') <- evalExp env' e2
            case (v1,v2) of
                (VInt i1, VInt i2)       -> return (VInt (i1*i2), env'')
                (VDouble d1, VDouble d2) -> return (VDouble (d1*d2), env'')
        EDiv e1 e2   -> do
            (v1, env')  <- evalExp env e1
            (v2, env'') <- evalExp env' e2
            case (v1,v2) of
                (VInt i1, VInt i2)       -> return (VInt (quot i1 i2), env'')
                (VDouble d1, VDouble d2) -> return (VDouble (d1/d2), env'')
        EPlus e1 e2   -> do
            (v1, env')  <- evalExp env e1
            (v2, env'') <- evalExp env' e2
            case (v1,v2) of
                (VInt i1, VInt i2)       -> return (VInt (i1+i2), env'')
                (VDouble d1, VDouble d2) -> return (VDouble (d1+d2), env'')
        EMinus e1 e2   -> do
            (v1, env')  <- evalExp env e1
            (v2, env'') <- evalExp env' e2
            case (v1,v2) of
                (VInt i1, VInt i2)       -> return (VInt (i1-i2), env'')
                (VDouble d1, VDouble d2) -> return (VDouble (d1-d2), env'')
        ELt e1 e2     -> do
            (v1, env') <- evalExp env e1
            (v2, env'') <- evalExp env' e2
            if v1 < v2
                then return (VBool True, env'')
                else return (VBool False, env'')
        EGt e1 e2     -> do
            (v1, env') <- evalExp env e1
            (v2, env'') <- evalExp env' e2
            if v1 > v2
                then return (VBool True, env'')
                else return (VBool False, env'')
        ELtEq e1 e2     -> do
            (v1, env') <- evalExp env e1
            (v2, env'') <- evalExp env' e2
            if v1 <= v2
                then return (VBool True, env'')
                else return (VBool False, env'')
        EGtEq e1 e2     -> do
            (v1, env') <- evalExp env e1
            (v2, env'') <- evalExp env' e2
            if v1 >= v2
                then return (VBool True, env'')
                else return (VBool False, env'')
        EEq e1 e2     -> do
            (v1, env') <- evalExp env e1
            (v2, env'') <- evalExp env' e2
            if v1 == v2
                then return (VBool True, env'')
                else return (VBool False, env'')
        ENEq e1 e2     -> do
            (v1, env') <- evalExp env e1
            (v2, env'') <- evalExp env' e2
            if v1 /= v2
                then return (VBool True, env'')
                else return (VBool False, env'')
        EAnd e1 e2    -> do
            (v1, env') <- evalExp env e1
            if v1 == VBool True
                then evalExp env' e2
                else return (VBool False, env')
        EOr e1 e2    -> do
            (v1, env') <- evalExp env e1
            if v1 == VBool False
                then evalExp env' e2
                else return (VBool True, env')
        EAss (EId i) e2    -> do
            (val, env')  <- evalExp env e2
            return (val, setVar env' i val)
  where foldEval (val, env) e = do
            (val', env') <- evalExp env e
            return (val ++ [val'], env')

data Value = VInt Integer
           | VDouble Double
           | VBool Bool
           | VVoid
           | VUndef
           deriving (Eq, Show)

instance Ord Value where
    compare (VInt i1) (VInt i2) = compare i1 i2
    compare (VInt i) (VDouble d) = compare (fromIntegral i) d
    compare (VDouble d) (VInt i) = compare d (fromIntegral i)
    compare (VDouble d) (VDouble d2) = compare d d2

type Fun = Map.Map Id Def
type Context = Map.Map Id Value
data Env = Env { fun :: Fun
               , context :: [Context]
               }

emptyEnv :: Env
emptyEnv = Env Map.empty ([Map.empty])

addVar :: Env -> Id -> Env
addVar (Env s (c:cs)) i = Env s (Map.insert i VUndef c:cs)

{-
setVar :: Env -> Id -> Value -> Env
setVar (Env s (c:cs)) i v = Env s (Map.insert i v c:cs)
-}
setVar :: Env -> Id -> Value -> Env
setVar (Env s []) i v = error $ "Unknown variable " ++ printTree i ++ "."
setVar (Env s (c:cs)) i v =
    case Map.lookup i c of
        Nothing -> setVar (Env s cs) i v
        Just x -> Env s (Map.insert i v c:cs)

setFun :: Env -> Def -> Env
setFun (Env s c) d@(DFun _ i _ _) = Env (Map.insert i d s) c

lookupVar :: Env -> Id -> Value
lookupVar (Env _ []) i = error $ "Unknown variable " ++ printTree i ++ "."
lookupVar (Env s (c:cs)) i =
    case Map.lookup i c of
        Nothing -> lookupVar (Env s cs) i
        Just v -> v

lookupFun :: Env -> Id -> Def
lookupFun (Env s _) i =
    case Map.lookup i s of
        Nothing -> error $ "Function " ++ show i ++ " not found."
        Just d -> d

enterScope :: Env -> Env
enterScope (Env s c) = Env s (Map.empty:c)

leaveScope :: Env -> Env
leaveScope (Env s (c:cs)) = Env s cs
