module CodeGenerator where

import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State

import AbsCPP

compile :: String -> Program -> String
compile = undefined

compileProgram :: String -> Program -> State Env ()
compileProgram name (PDefs defs) = do
    mapM_ emit [
      ".class public " ++ name,
      ".super java/lang/Object",
      "",
      ".method public <init>()V",
      "aload_0",
      "invokenonvirtual java/lang/Object/<init>()V",
      "return",
      ".end method",
      "",
      ".method public static main([Ljava/lang/String;])V",
      ".limit locals 100",  --- bogus limit
      ".limit stack 1000"   --- bogus limit
     ]
    mapM_ compileDef defs
    emit "return"
    emit ".end method"

compileDef :: Def -> State Env ()
compileDef = undefined

compileStm :: Stm -> State Env ()
compileStm s =
    case s of
        SExp e -> undefined
        SDecls t is -> undefined
        SInit t i e -> undefined
        SReturn e -> undefined
        SWhile e s -> undefined
        SBlock stms -> undefined
        SIfElse e s1 s2 -> undefined

compileExp :: Exp -> State Env ()
compileExp e =
    case e of
        ETrue -> undefined
        EFalse -> undefined
        EInt i -> undefined
        EDouble d -> undefined
        EId i -> undefined
        EApp i exps -> undefined
        EPIncr e -> undefined
        EPDecr e -> undefined
        EIncr e -> undefined
        EDecr e -> undefined
        ETimes a b -> do
            compileExp a
            compileExp b
            emit $ case typExp e of
                Type_int -> imul_Instr
                Type_double -> dmul_Instr
        EDiv e1 e2 -> undefined
        EPlus e1 e2 -> undefined
        EMinus e1 e2 -> undefined
        ELt e1 e2 -> undefined
        EGt e1 e2 -> undefined
        ELtEq e1 e2 -> undefined
        EGtEq e1 e2 -> undefined
        EEq e1 e2 -> undefined
        ENEq e1 e2 -> undefined
        EAnd e1 e2 -> undefined
        EOr e1 e2 -> undefined
        EAss e1 e2 -> undefined


-- might require type-annotated syntax tree
typExp :: Exp -> Type
typExp = undefined

-- emit :: Code -> State Env ()
emit :: Instruction -> State Env ()
emit i = modify (\s -> s {code = i : code s})

data Env = Env {
    vars        :: [Map.Map Id Address],
    nextLabel   :: Int,
    nextAddress :: Address,
    maxAddress  :: Address,
    stackSize   :: Int,
    maxSize     :: Int,
    code        :: [Instruction]
} deriving (Show)

type Instruction = String
type Address = Int
type FunType = Int
type Label = String

emptyEnv :: Env
emptyEnv = Env {
    vars        = [Map.empty],
    nextLabel   = 0,
    nextAddress = 1,
    maxAddress  = 1,
    stackSize   = 0,
    maxSize     = 1,
    code        = []
    }

lookupVar :: Id -> State Env Address
lookupVar x = do
    s <- get
    -- then look up the first occurence of x in (vars s)
    undefined

lookupFun :: Id -> State Env FunType
lookupFun = undefined

newBlock :: State Env ()
newBlock = undefined

exitBlock :: State Env ()
exitBlock = undefined

newLabel :: State Env Label
newLabel = undefined





-- Instructions
imul_Instr = "imul"
dmul_Instr = "dmul"

-- addresses start from 0
-- each new variable gets a new address
-- the next address is incremented by THE SIZE OF THE VARIABLE
-- int, bool, strings: size=1    double: size=2
-- the first variables are the function parameters, after which the locals follow
-- 
