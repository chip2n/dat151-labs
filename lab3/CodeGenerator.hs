module CodeGenerator where

import qualified Data.Map as Map
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
      "",
      ".method public static main([Ljava/lang/String;)V",
      ".limit locals 100",  --- bogus limit
      ".limit stack 1000",   --- bogus limit
      "invokestatic test/main2()I",
      "return",
      ".end method"
     ]
    mapM_ compileDef defs

compileDef :: Def -> State Env ()
compileDef (DFun _ (Id "main") _ stms) = do
    emit $ ".method public static main2()I"
    emit $ ".limit locals 100"  --- bogus limit
    emit $ ".limit stack 1000"   --- bogus limit
    mapM_ compileStm $ stms
    emit $ "iconst_0"  -- maybe hax
    emit $ "ireturn"
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
        typArgs = concat $ map (\(ADecl t' _) -> typ t') args

compileStm :: Stm -> State Env ()
compileStm s = case s of
    SExp    e       -> do
        compileExp e
        case typExp e of
            Type_int -> emit "pop"
            Type_bool -> emit "pop"
            Type_double -> emit "pop2"
            Type_void -> return ()
    SDecls  t is    -> do
        mapM_ (\i -> addVar i t) is
    SInit   t i e   -> do
        addVar i t
        a <- lookupVar i
        compileExp e
        emit $ "istore " ++ show a
    SReturn e       -> do
        compileExp e
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
    SIfElse e s1 s2 -> ifElse e (compileStm s1) (compileStm s2)

compileExp :: Exp -> State Env ()
compileExp (ETyped t e) = case e of
    ETrue       -> emit "iconst_1"
    EFalse      -> emit "iconst_0"
    EInt i      -> emit $ "bipush " ++ show i
    EDouble d   -> emit $ "ldc2_w " ++ show d
    EId i       -> do
        a <- lookupVar i
        emit $ case t of
            Type_int    -> "iload " ++ show a
            Type_bool   -> "iload " ++ show a
            Type_double -> "dload " ++ show a
    EApp (Id "printInt")    e' -> do
        mapM_ compileExp e'
        emit $ "invokestatic Runtime/printInt(I)V"
    EApp (Id "printDouble") e' -> do
        mapM_ compileExp e'
        emit $ "invokestatic Runtime/printDouble(D)V"
    EApp (Id "readInt")    _ -> do
        emit $ "invokestatic Runtime/readInt()I"
    EApp (Id "readDouble") _ -> do
        emit $ "invokestatic Runtime/readDouble()D"
    EApp   i es -> error "yolo"
    -- Unary arithmetics
    EPIncr e'@(ETyped t' (EId i))    -> do
        compileExp e'
        a <- lookupVar i
        case t' of
            Type_int -> do
                emit $ "dup"
                emit $ "iconst_1"
                emit $ "iadd"
                emit $ "istore " ++ show a
    EPDecr e'@(ETyped t' (EId i))    -> do
        compileExp e'
        a <- lookupVar i
        case t' of
            Type_int -> do
                emit $ "dup"
                emit $ "iconst_1"
                emit $ "isub"
                emit $ "istore " ++ show a
    EIncr e'@(ETyped t' (EId i))    -> do
        compileExp e'
        a <- lookupVar i
        case t' of
            Type_int -> do
                emit $ "iconst_1"
                emit $ "iadd"
                emit $ "dup"
                emit $ "istore " ++ show a
    EDecr  e     -> error "Not defined: EDecr"
    -- Binary arithmetics
    ETimes e1 e2 -> compileArithmetic e1 e2 "mul" t
    EDiv   e1 e2 -> compileArithmetic e1 e2 "div" t
    EPlus  e1 e2 -> compileArithmetic e1 e2 "add" t
    EMinus e1 e2 -> compileArithmetic e1 e2 "sub" t
    -- Comparisons
    ELt    e1 e2 -> compareExp e1 e2 "if_icmplt"
    EGt    e1 e2 -> compareExp e1 e2 "if_icmpgt"
    ELtEq  e1 e2 -> compareExp e1 e2 "if_icmple"
    EGtEq  e1 e2 -> compareExp e1 e2 "if_icmpge"
    EEq    e1 e2 -> compareExp e1 e2 "if_icmpeq"
    ENEq   e1 e2 -> compareExp e1 e2 "if_icmpne"
    EAnd   e1 e2 -> 
        ifElse e1 (
            ifElse e2
                (emit $ "iconst_1")
                (emit $ "iconst_0"))
            (emit $ "iconst_0")
    EOr   e1 e2  -> 
        ifElse e1 (
            emit $ "iconst_1")
            (ifElse e2 (emit $ "iconst_1") (emit $ "iconst_0"))
    EAss  (ETyped _ (EId i)) e2  -> do
        compileExp e2
        a <- lookupVar i
        case t of
            Type_int -> do
                emit $ "dup"
                emit $ "istore " ++ show a
            Type_double -> do
                emit $ "dup2"
                emit $ "dstore " ++ show a
            Type_bool -> do
                emit $ "dup"
                emit $ "istore " ++ show a
compileExp e = error $ "Not ETyped: " ++ show e

compareExp :: Exp -> Exp -> Instruction -> State Env ()
compareExp e1 e2 i = do
    true <- newLabel
    let trueLabel = "l" ++ show true
    emit "bipush 1"
    compileExp e1
    compileExp e2
    emit $ i ++ " " ++ trueLabel
    emit "pop"
    emit "bipush 0"
    emit $ trueLabel ++ ":"

compileArithmetic :: Exp -> Exp -> Instruction -> Type -> State Env ()
compileArithmetic e1 e2 i t = do
    compileExp e1
    compileExp e2
    emit $ case t of
        Type_int    -> "i" ++ i
        Type_double -> "d" ++ i
    

ifElse :: Exp -> State Env () -> State Env () -> State Env ()
ifElse e s1 s2 = do
    false <- newLabel
    true <- newLabel
    let falseLabel = "l" ++ show false
    let trueLabel = "l" ++ show true
    compileExp e 
    emit $ "ifeq " ++ falseLabel
    s1
    emit $ "goto " ++ trueLabel
    emit $ falseLabel ++ ":"
    s2
    emit $ trueLabel ++ ":"

typExp :: Exp -> Type
typExp (ETyped t _) = t

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

size :: Type -> Int
size Type_int    = 1
size Type_bool   = 1
size Type_double = 2
