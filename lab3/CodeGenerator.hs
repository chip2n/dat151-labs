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
compileDef (DFun t i args stms) = do
    emit $ ".method public static " ++ show i ++ "(" ++ typArgs ++ ")" ++ typ t
    emit $ ".limit locals " ++ show (length typArgs)  -- TODO: Probably wrong, see p.39
    emit $ ".limit stack " ++ show (length typArgs)
    -- TODO: Add variables
    mapM_ compileStm $ stms -- TODO: Dont know if works
    emit ".end method"
  where typ (Type_int)    = "I"
        typ (Type_double) = "D"
        typ (Type_void)   = "V"
        typ (Type_bool)   = "Z"
        typArgs = concat $ map (\(ADecl t i) -> typ t) args

compileStm :: Stm -> State Env ()
compileStm s =
    case s of
        SExp    e       -> undefined
        SDecls  t is    -> undefined
        SInit   t i e   -> undefined
        SReturn e       -> undefined
        SWhile  e s'    -> do
            test <- newLabel
            end  <- newLabel
            emit $ show test ++ ":"
            compileExp e
            emit $ "ifeq " ++ show end
            compileStm s'
            emit $ "goto " ++ show test
            emit $ show end ++ ":"
        SBlock  stms    -> undefined
        SIfElse e s1 s2 -> do
            false <- newLabel
            true <- newLabel
            compileExp e
            emit $ "ifeq " ++ show false
            compileStm s1
            emit $ "goto " ++ show true
            emit $ show false ++ ":"
            compileStm s2
            emit $ show true ++ ":"

compileExp :: Exp -> State Env ()
compileExp e =
    case e of
        ETrue       -> emit "iconst_1"
        EFalse      -> emit "iconst_0"
        EInt i      -> emit $ "bipush " ++ show i
        EDouble d   -> emit $ "ldc2_w " ++ show d
        EId i       -> do
            a <- lookupVar i
            emit $ case typExp e of
                Type_int    -> "iload " ++ show a
                Type_double -> "dload " ++ show a
        EApp   i es -> undefined
        EPIncr e    -> undefined
        EPDecr e    -> undefined
        EIncr  e    -> undefined
        EDecr  e    -> undefined
        ETimes a b  -> do
            compileExp a
            compileExp b
            emit $ case typExp e of
                Type_int    -> "imul"
                Type_double -> "dmul"
        EDiv e1 e2   -> do
            compileExp e1
            compileExp e2
            emit $ case typExp e of
                Type_int    -> "idiv"
                Type_double -> "ddiv"
        EPlus e1 e2  -> do
            compileExp e1
            compileExp e2
            emit $ case typExp e of
                Type_int    -> "iadd"
                Type_double -> "dadd"
        EMinus e1 e2 -> do
            compileExp e1
            compileExp e2
            emit $ case typExp e of
                Type_int    -> "isub"
                Type_double -> "dsub"
        ELt   e1 e2  -> do
            true <- newLabel
            emit "bipush 1"
            compileExp e1
            compileExp e2
            emit $ "if_icmplt " ++ show true
            emit "pop"
            emit "bipush 0"
            emit $ show true ++ ":"
        EGt   e1 e2  -> undefined
        ELtEq e1 e2  -> undefined
        EGtEq e1 e2  -> undefined
        EEq   e1 e2  -> undefined
        ENEq  e1 e2  -> undefined
        EAnd  e1 e2  -> undefined
        EOr   e1 e2  -> undefined
        EAss  e1 e2  -> undefined


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
type Label = Int

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
    return $ find (vars s)
  where find [] = error $ "No variable " ++ show x ++ " found"
        find (m:ms) = do
            case Map.lookup x m of
                Just a  -> a
                Nothing -> find ms

lookupFun :: Id -> State Env FunType
lookupFun = undefined

newBlock :: State Env ()
newBlock = modify (\s -> s { vars = Map.empty:vars s })

exitBlock :: State Env ()
exitBlock = modify (\s -> s { vars = tail $ vars s })

newLabel :: State Env Label
newLabel = do
    s <- get
    let label = nextLabel s
    modify (\s' -> s' { nextLabel = label + 1 })
    return label

addVar :: Id -> Type -> State Env ()
addVar i t = do
    s <- get
    let (currS:restS) = vars s
    modify (\s' -> s'
            { nextAddress = (nextAddress s') + size t
            , vars = Map.insert i (nextAddress s') currS:restS
            }
           )
    return ()

size :: Type -> Int
size Type_int    = 1
size Type_bool   = 1
size Type_double = 2

-- the first variables are the function parameters, after which the locals follow
