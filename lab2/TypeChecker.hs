module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM


typecheck :: Program -> Err ()
typecheck (PDefs defs) = checkDefs emptyEnv defs

checkDefs :: Env -> [Def] -> Err ()
checkDefs = undefined

checkDef :: Env -> Def -> Err Env
checkDef = undefined

checkStms :: Env -> [Stm] -> Err ()
checkStms = undefined

checkStm :: Env -> Stm -> Err Env
checkStm = undefined

checkExp :: Env -> Exp -> Type -> Err ()
checkExp = undefined

inferExp :: Env -> Exp -> Err Type
inferExp = undefined







type Env = [[(Id, Type)]]

emptyEnv :: Env
emptyEnv = [[]]

addVar :: Env -> Id -> Type -> Err Env
addVar = undefined

lookupVar :: Env -> Id -> Type -> Err Env
lookupVar = undefined

addScope :: Env -> Env
addScope = undefined
