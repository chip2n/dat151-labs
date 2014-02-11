module TypeChecker where

import qualified Data.Map as Map
import Control.Monad

import AbsCPP
import PrintCPP
import ErrM


typecheck :: Program -> Err ()
typecheck (PDefs defs) = do
    env <- buildInitialSymbolTable emptyEnv
    env' <- buildSymbolTable env defs
    checkDefs env' defs 
    return ()

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
    
checkDefs :: Env -> [Def] -> Err Env
checkDefs = foldM checkDef

checkDef :: Env -> Def -> Err Env
checkDef env (DFun r _ args stms) = do
    env' <- foldM addArg (newBlock env) args
    checkStms (newEnv env') stms
  where addArg e (ADecl t i) = addVar e i t
        newEnv (Env s c _) = Env s c r

checkStms :: Env -> [Stm] -> Err Env
checkStms = foldM checkStm

checkStm :: Env -> Stm -> Err Env
checkStm env s =
    case s of
        SExp e -> inferExp env e >> return env
        SDecls t xs -> foldM (addFold t) env xs
        SInit t n e -> checkExp env e t >> addVar env n t
        SReturn e -> do
            t <- inferExp env e
            if (returnType env) == t
                then return env
                else fail "yoloo"
        SWhile e s1 -> checkExp env e Type_bool >> checkStm env s1
        SBlock stms -> checkStms (newBlock env) stms >> return env
        SIfElse e s1 s2 -> do
            checkExp env e Type_bool
            checkStm env s1
            checkStm env s2
            return env
  where addFold t a b = addVar a b t

checkExp :: Env -> Exp -> Type -> Err ()
checkExp env e t = do
    t' <- inferExp env e
    if t' /= t
        then fail (printTree e ++ " has type " ++ printTree t'
                   ++ " expected " ++ printTree t)
        else return ()

inferExp :: Env -> Exp -> Err Type
inferExp env e =
    case e of
        EInt _    -> return Type_int
        EDouble _ -> return Type_double
        ETrue     -> return Type_bool
        EFalse    -> return Type_bool
        EId x     -> lookupVar env x
        EApp x xs -> do
            (argTypes, ret) <- lookupFun env x
            xs' <- mapM (inferExp env) xs
            if argTypes == xs'
                then return ret
                else fail (printTree x ++ " called with arguments"
                            ++ show xs ++ ", but the required arguments are"
                            ++ show argTypes)
        EPIncr x  -> inferUna [Type_int, Type_double] env x
        EPDecr x  -> inferUna [Type_int, Type_double] env x
        EIncr x  -> inferUna [Type_int, Type_double] env x
        EDecr x  -> inferUna [Type_int, Type_double] env x
        ETimes e1 e2 -> compareExp e1 e2
        EDiv e1 e2 -> compareExp e1 e2
        EPlus e1 e2 -> inferBin [Type_int, Type_double] env e1 e2
        EMinus e1 e2 -> inferBin [Type_int, Type_double] env e1 e2
        ELt e1 e2 -> compareExp e1 e2 >> return Type_bool
        EGt e1 e2 -> compareExp e1 e2 >> return Type_bool
        ELtEq e1 e2 -> compareExp e1 e2 >> return Type_bool
        EGtEq e1 e2 -> compareExp e1 e2 >> return Type_bool
        EEq e1 e2 -> compareExp e1 e2 >> return Type_bool
        ENEq e1 e2 -> compareExp e1 e2 >> return Type_bool
        EAnd e1 e2 -> do
            t1 <- inferExp env e1
            t2 <- inferExp env e2
            if t1 == Type_bool && t2 == Type_bool
                then return t1
                else fail (printTree e1 ++ " has type " ++ printTree t1
                            ++ " and " ++ printTree e2 ++ " has type "
                            ++ printTree t2 ++ ", but conjunction requires"
                            ++ " both arguments to be of type Bool.")
        EOr e1 e2 -> do
            t1 <- inferExp env e1
            t2 <- inferExp env e2
            if t1 == Type_bool && t2 == Type_bool
                then return t1
                else fail (printTree e1 ++ " has type " ++ printTree t1
                            ++ " and " ++ printTree e2 ++ " has type "
                            ++ printTree t2 ++ ", but disjunction requires"
                            ++ " both arguments to be of type Bool.")
        EAss (EId x) e1 -> compareExp (EId x) e1 >> inferExp env (EId x)
  where compareExp e1 e2 = do t1 <- inferExp env e1
                              t2 <- inferExp env e2
                              if t1 == t2
                                  then return t1
                                  else fail (printTree e1 ++ " has type " ++ printTree t1
                                              ++ " but " ++ printTree e2
                                              ++ " has type " ++ printTree t2)

inferBin :: [Type] -> Env -> Exp -> Exp -> Err Type
inferBin types env e1 e2 = do
    typ <- inferExp env e1
    if elem typ types
        then checkExp env e2 typ >> return typ
        else fail $ "Wrong type of expression" ++ printTree e1

inferUna :: [Type] -> Env -> Exp -> Err Type
inferUna types env e = do
    typ <- inferExp env e
    if elem typ types
        then return typ
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
