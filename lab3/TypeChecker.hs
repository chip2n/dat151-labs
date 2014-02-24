module TypeChecker where

import qualified Data.Map as Map
import Control.Monad

import AbsCPP
import PrintCPP
import ErrM


typecheck :: Program -> Err Program
typecheck (PDefs defs) = do
    env <- buildInitialSymbolTable emptyEnv
    env' <- buildSymbolTable env defs
    (env'', defs') <- checkDefs env' defs 
    return (PDefs defs')

buildInitialSymbolTable :: Env -> Err Env
buildInitialSymbolTable env = do
    env <- updateFun env (Id "printInt") ([Type_int], Type_void)
    env <- updateFun env (Id "printDouble") ([Type_double], Type_void)
    env <- updateFun env (Id "readInt") ([], Type_int)
    env <- updateFun env (Id "readDouble") ([], Type_int)
    return env

buildSymbolTable :: Env -> [Def] -> Err Env
buildSymbolTable env [] = return env
buildSymbolTable env ((DFun t id args stms):ds) = do
    env' <- updateFun env id (map getType args, t)
    buildSymbolTable env' ds
  where getType (ADecl x _) = x
    
checkDefs :: Env -> [Def] -> Err (Env, [Def])
checkDefs env defs = foldM dFold (env,[]) defs
    where dFold (env, ds) d = do
              (env',d') <- checkDef env d
              return (env', ds ++ [d'])

checkDef :: Env -> Def -> Err (Env, Def)
checkDef env (DFun r a args stms) = do
    env' <- foldM addArg (newBlock env) args
    (env'', stms') <- checkStms (newEnv env') stms
    return (env'', DFun r a args stms')
  where addArg e (ADecl t i) = addVar e i t
        newEnv (Env s c _) = Env s c r

checkStms :: Env -> [Stm] -> Err (Env, [Stm])
checkStms env stms = foldM cFold (env, []) stms
    where cFold (env, ss) s = do
              (env',s') <- checkStm env s
              return (env', ss ++ [s'])

checkStm :: Env -> Stm -> Err (Env, Stm)
checkStm env s =
    case s of
        SExp e -> do
            e' <- inferExp env e
            return (env, (SExp e'))
        SDecls t xs -> do
            env' <- foldM (addFold t) env xs
            return (env', s)
        SInit t n e -> do
            e' <- checkExp env e t
            env' <- addVar env n t
            return (env', (SInit t n e'))
        SReturn e -> do
            ETyped t e' <- inferExp env e
            if (returnType env) == t
                then return (env, SReturn (ETyped t e'))
                else fail $ "Wrong return type, expected " ++ show (returnType env)
        SWhile e s1 -> do
            e' <- checkExp env e Type_bool
            (_,s1') <- checkStm env s1
            return (env, SWhile e' s1')
        SBlock stms -> do
            (env', stms') <- checkStms (newBlock env) stms
            return (env, (SBlock stms'))
        SIfElse e s1 s2 -> do
            e' <- checkExp env e Type_bool
            (_,s1') <- checkStm env s1
            (_,s2') <- checkStm env s2
            return (env, (SIfElse e' s1' s2'))
  where addFold t a b = addVar a b t

checkExp :: Env -> Exp -> Type -> Err Exp
checkExp env e t = do
    ETyped t' e' <- inferExp env e
    if t' /= t
        then fail (printTree e ++ " has type " ++ printTree t'
                   ++ " expected " ++ printTree t)
        else return (ETyped t' e')

inferExp :: Env -> Exp -> Err Exp
inferExp env e =
    case e of
        EInt _    -> return (ETyped Type_int e)
        EDouble _ -> return (ETyped Type_double e)
        ETrue     -> return (ETyped Type_bool e)
        EFalse    -> return (ETyped Type_bool e)
        EId x     -> do
            t <- lookupVar env x
            return $ ETyped t e
        EApp x xs -> do
            (argTypes, ret) <- lookupFun env x
            xs' <- mapM (inferExp env) xs
            let xs'' = map (\(ETyped t e') -> t) xs'
            if argTypes == xs''
                then return (ETyped ret (EApp x xs'))
                else fail (printTree x ++ " called with arguments"
                            ++ show xs ++ ", but the required arguments are"
                            ++ show argTypes)
        EPIncr x  -> do
            e'@(ETyped t _) <- inferUna [Type_int, Type_double] env x
            return (ETyped t (EPIncr e'))
        EPDecr x  -> do
            e'@(ETyped t _) <- inferUna [Type_int, Type_double] env x
            return (ETyped t (EPDecr e'))
        EIncr x  -> do
            e'@(ETyped t _) <- inferUna [Type_int, Type_double] env x
            return (ETyped t (EIncr e'))
        EDecr x  -> do
            e'@(ETyped t _) <- inferUna [Type_int, Type_double] env x
            return (ETyped t (EDecr e'))
        ETimes e1 e2 -> do
            t <- compareExp e1 e2
            return (ETyped t (ETimes (ETyped t e1) (ETyped t e2)))
        EDiv e1 e2 -> do
            t <- compareExp e1 e2
            return (ETyped t (EDiv (ETyped t e1) (ETyped t e2)))
        EPlus e1 e2 -> do
            ETyped t _ <- inferBin [Type_int, Type_double] env e1 e2
            return (ETyped t (EPlus (ETyped t e1) (ETyped t e2)))
        EMinus e1 e2 -> do
            ETyped t _ <- inferBin [Type_int, Type_double] env e1 e2
            return (ETyped t (EMinus (ETyped t e1) (ETyped t e2)))
        ELt e1 e2 -> do
            compareExp e1 e2
            e1' <- inferExp env e1
            e2' <- inferExp env e2
            return (ETyped Type_bool (ELt e1' e2'))
        EGt e1 e2 -> do
            compareExp e1 e2
            e1' <- inferExp env e1
            e2' <- inferExp env e2
            return (ETyped Type_bool (EGt e1' e2'))
        ELtEq e1 e2 -> do
            compareExp e1 e2
            e1' <- inferExp env e1
            e2' <- inferExp env e2
            return (ETyped Type_bool (ELtEq e1' e2'))
        EGtEq e1 e2 -> do
            compareExp e1 e2
            e1' <- inferExp env e1
            e2' <- inferExp env e2
            return (ETyped Type_bool (EGtEq e1' e2'))
        EEq e1 e2 -> do
            compareExp e1 e2
            e1' <- inferExp env e1
            e2' <- inferExp env e2
            return (ETyped Type_bool (EEq e1' e2'))
        ENEq e1 e2 -> do
            compareExp e1 e2
            e1' <- inferExp env e1
            e2' <- inferExp env e2
            return (ETyped Type_bool (ENEq e1' e2'))
        EAnd e1 e2 -> do
            (ETyped t1 _) <- inferExp env e1
            (ETyped t2 _) <- inferExp env e2
            if t1 == Type_bool && t2 == Type_bool
                then return (ETyped t1 e)
                else fail (printTree e1 ++ " has type " ++ printTree t1
                            ++ " and " ++ printTree e2 ++ " has type "
                            ++ printTree t2 ++ ", but conjunction requires"
                            ++ " both arguments to be of type Bool.")
        EOr e1 e2 -> do
            (ETyped t1 _) <- inferExp env e1
            (ETyped t2 _) <- inferExp env e2
            if t1 == Type_bool && t2 == Type_bool
                then return (ETyped t1 e)
                else fail (printTree e1 ++ " has type " ++ printTree t1
                            ++ " and " ++ printTree e2 ++ " has type "
                            ++ printTree t2 ++ ", but disjunction requires"
                            ++ " both arguments to be of type Bool.")
        EAss (EId x) e1 -> do
            compareExp (EId x) e1
            ETyped t _ <- inferExp env (EId x)
            e1' <- inferExp env e1
            return (ETyped t (EAss (ETyped t (EId x)) e1'))
  where compareExp e1 e2 = do (ETyped t1 _) <- inferExp env e1
                              (ETyped t2 _) <- inferExp env e2
                              if t1 == t2
                                  then return t1
                                  else fail (printTree e1 ++ " has type " ++ printTree t1
                                              ++ " but " ++ printTree e2
                                              ++ " has type " ++ printTree t2)

inferBin :: [Type] -> Env -> Exp -> Exp -> Err Exp
inferBin types env e1 e2 = do
    (ETyped typ x) <- inferExp env e1
    if elem typ types
        then checkExp env e2 typ >> return (ETyped typ x)
        else fail $ "Wrong type of expression" ++ printTree e1

inferUna :: [Type] -> Env -> Exp -> Err Exp
inferUna types env e = do
    (ETyped typ x) <- inferExp env e
    if elem typ types
        then return (ETyped typ x)
        else fail $ "Wrong type of expression" ++ printTree e


type Sig = Map.Map Id ([Type], Type)
type Context = Map.Map Id Type

data Env = Env { sig :: Sig
               , context :: [Context]
               , returnType :: Type
               } 

emptyEnv :: Env
emptyEnv = Env Map.empty [Map.empty] Type_void

addVar :: Env -> Id -> Type -> Err Env
addVar (Env sig (c:cs) ret) i t = do
    if i `Map.notMember` c
        then return $ Env sig (Map.insert i t c:cs) ret
        else fail $ "Variable " ++ show i
                     ++ " already declared in this context."

lookupVar :: Env -> Id -> Err Type
lookupVar (Env _ context _) i = lookupVarContext context i

lookupVarContext :: [Context] -> Id -> Err Type
lookupVarContext [] i = fail $ "Variable " ++ show i ++ " not found"
lookupVarContext (c:cs) i = do
    case Map.lookup i c of
        Nothing -> lookupVarContext cs i
        Just t  -> return t
    
lookupFun :: Env -> Id -> Err ([Type], Type)
lookupFun (Env sig _ _) i = do
    case Map.lookup i sig of
        Nothing -> fail $ "Function " ++ show i ++ " not found."
        Just f  -> return f

updateFun :: Env -> Id -> ([Type], Type) -> Err Env
updateFun (Env sig x ret) i s = do
    case Map.lookup i sig of
        Nothing -> do
            let sig' = Map.insert i s sig
            return $ Env sig' x ret
        Just _  -> fail $ "Function " ++ show i ++ " already in symbol table."

newBlock :: Env -> Env
newBlock (Env sig cs ret) = Env sig (Map.empty:cs) ret
