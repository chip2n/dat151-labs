module CodeGenerator where

import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State

import AbsCPP

compile :: String -> Program -> String
compile s p = unlines $ reverse $ code (execState (compileProgram s p) emptyEnv)

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
      ""
     ]
    mapM_ compileDef defs

compileDef :: Def -> State Env ()
compileDef (DFun t (Id "main") args stms) = do
    emit $ ".method public static main([Ljava/lang/String;)V"
    emit $ ".limit locals 100"  --- bogus limit
    emit $ ".limit stack 1000"   --- bogus limit
    mapM_ compileStm $ stms
    emit "return"
    emit ".end method"
compileDef (DFun t (Id i) args stms) = do
    emit $ ".method public static " ++ i ++ "(" ++ typArgs ++ ")" ++ typ t
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
        SExp    e       -> do
            compileExp e
            case typExp e of
                Type_int -> emit "pop"
                Type_bool -> emit "pop"
                Type_double -> emit "pop2"
                Type_void -> return ()
        SDecls  t is    -> undefined
        SInit   t i e   -> do
            addVar i t
            a <- lookupVar i
            compileExp e
            emit $ "istore " ++ show a
        SReturn e       -> 
            case typExp e of
                Type_int    -> emit "ireturn"
                Type_double -> emit "dreturn"
        SWhile  e s'    -> do
            test <- newLabel
            end  <- newLabel
            let testLabel = "l" ++ show test
            let endLabel = "l" ++ show end

            emit $ testLabel ++ ":"
            compileExp e
            emit $ "ifeq " ++ endLabel
            compileStm s'
            emit $ "goto " ++ testLabel
            emit $ endLabel ++ ":"
        SBlock  stms    -> mapM_ compileStm stms
        SIfElse e s1 s2 -> do
            false <- newLabel
            true <- newLabel
            let falseLabel = "l" ++ show false
            let trueLabel = "l" ++ show true
            compileExp e 
            emit $ "ifeq " ++ falseLabel
            compileStm s1
            emit $ "goto " ++ trueLabel
            emit $ falseLabel ++ ":"
            compileStm s2
            emit $ trueLabel ++ ":"

compileExp :: Exp -> State Env ()
compileExp (ETyped t e) =
    case e of
        ETrue       -> emit "iconst_1"
        EFalse      -> emit "iconst_0"
        EInt i      -> emit $ "bipush " ++ show i
        EDouble d   -> emit $ "ldc2_w " ++ show d
        EId i       -> do
            a <- lookupVar i
            emit $ case t of
                Type_int    -> "iload " ++ show a
                Type_double -> "dload " ++ show a
        EApp (Id "printInt") e' -> do
            mapM_ compileExp e'
            emit $ "invokestatic Runtime/printInt(I)V"
        EApp (Id "printDouble") e' -> do
            mapM_ compileExp e'
            emit $ "invokestatic Runtime/printDouble(D)V"
        EApp (Id "readInt") e' -> do
            emit $ "invokestatic Runtime/readInt()I"
        EApp (Id "readDouble") e' -> do
            emit $ "invokestatic Runtime/readDouble()D"
        EApp   i es -> error "yolo"
        EPIncr e    -> undefined
        EPDecr e -> undefined
        EIncr e'@(ETyped t' (EId i))    -> do
            compileExp e'
            a <- lookupVar i
            case t' of
                Type_int -> do
                    emit $ "iconst_1"
                    emit $ "iadd"
                    emit $ "dup"
                    emit $ "istore " ++ show a
                Type_double -> do
                    emit $ "dconst_1"
                    emit $ "dadd"
                    -- TODO
        EDecr  e    -> undefined
        ETimes a b  -> do
            compileExp a
            compileExp b
            emit $ case t of
                Type_int    -> "imul"
                Type_double -> "dmul"
        EDiv e1 e2   -> do
            compileExp e1
            compileExp e2
            emit $ case t of
                Type_int    -> "idiv"
                Type_double -> "ddiv"
        EPlus e1 e2  -> do
            compileExp e1
            compileExp e2
            emit $ case t of
                Type_int    -> "iadd"
                Type_double -> "dadd"
        EMinus e1 e2 -> do
            compileExp e1
            compileExp e2
            emit $ case t of
                Type_int    -> "isub"
                Type_double -> "dsub"
        ELt   e1 e2  -> do
            true <- newLabel
            let trueLabel = "l" ++ show true
            emit "bipush 1"
            compileExp e1
            compileExp e2
            emit $ "if_icmplt " ++ trueLabel
            emit "pop"
            emit "bipush 0"
            emit $ trueLabel ++ ":"
        EGt   e1 e2  -> undefined
        ELtEq e1 e2  -> undefined
        EGtEq e1 e2  -> undefined
        EEq   e1 e2  -> undefined
        ENEq  e1 e2  -> undefined
        EAnd  e1 e2  -> undefined
        EOr   e1 e2  -> undefined
        EAss  (ETyped t' (EId i)) e2  -> do
            case t of
                Type_int -> do
                    a <- lookupVar i
                    emit $ "dup"
                    emit $ "istore " ++ show a
                Type_double -> undefined
compileExp e = error $ "Not ETyped: " ++ show e


-- might require type-annotated syntax tree
typExp :: Exp -> Type
typExp (ETyped t e) = t

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
