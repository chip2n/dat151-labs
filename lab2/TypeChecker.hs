module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM


typecheck :: Program -> Err ()
typecheck p = return ()

checkDefs :: Env -> [Def] -> Err ()
checkDefs = undefined

checkDef :: Env -> Degf -> Err Env
checkDef = undefined

checkStms :: Env -> [Stm] -> Err ()
checkStms = undefined

checkStm :: Env -> Stm -> Err Env
checkStm = undefined

checkExp :: Env -> Exp -> Type -> Err ()
checkExp = undefined

inferExp :: Env -> Exp -> Err Type
inferExp = undefined







type Env = [[(Ident, Type)]]

emptyEnv :: Env
emptyEnv = [[]]

addVar :: Env -> Ident -> Type -> Err Env
addVar = undefined

lookupVar :: Env -> Ident -> Type -> Err Env
lookupVar = undefined

addScope :: Env -> Env
addScope = undefined
