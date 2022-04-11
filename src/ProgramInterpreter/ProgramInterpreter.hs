{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
module ProgramInterpreter.ProgramInterpreter where

import ProgramInterpreter.ProgramInterpreterTypes
import ProgramInterpreter.Utils
import Environment.Environment
import Exception.RuntimeException
import Prelude
import Grammar.AbsLatte
import Control.Monad.Except
import Control.Monad.State

interpretProgram :: Program -> IO (Either RuntimeException SimpleType)
interpretProgram program = runExceptT $ evalStateT (runCode program) emptyEvalEnvironment

instance ProgramRunner Program where
  runCode (Program pos topDefs) = do
    mapM_ runCode topDefs
    -- eval main function
    return None

instance ProgramRunner TopDef where
  runCode (FnDef _ _ name args block) = do
    env <- get
    let fEnv = getFEnv env
    let fun = TFun args block fEnv
    modify $ putFunctionTypeValue name fun
    return None

instance ProgramRunner Block where
  runCode (Block _ stmts) = do
--    mapM_ runCode stmts
    return None


--data Stmt' a
--    = Empty a
--    | BStmt a (Block' a)
--    | Decl a (Type' a) [Item' a]
--    | Ass a Ident (Expr' a)
--    | Incr a Ident
--    | Decr a Ident
--    | Ret a (Expr' a)
--    | VRet a
--    | Cond a (Expr' a) (Stmt' a)
--    | CondElse a (Expr' a) (Stmt' a) (Stmt' a)
--    | While a (Expr' a) (Stmt' a)
--    | Break a
--    | Continue a
--    | SExp a (Expr' a)

--  runCode (FnDef _ _ name args block) = do
--    env <- get
--    let fEnv = getFEnv env
--    let fun = TFun args block fEnv
--    modify $ putFunctionTypeValue name fun
--    return None
--    data SimpleType = Int Integer | Str String | Bool Bool | None deriving (Eq, Show)
--    data Item' a = NoInit a Ident | Init a Ident (Expr' a)

--    putSimpleTypeValue :: Ident -> SimpleType -> EvalEnvironment -> EvalEnvironment
--    putSimpleTypeValue ident val env = EvalEnvironment {vEnv = vEnv', fEnv = fEnv env, store = store'}
--      where
--        (loc', store') = putStoreSimpleType val (store env)
--        vEnv' = putEnvLocation ident loc' (vEnv env)
--data Type' a
--    = Int a | Str a | Bool a | Void a | Fun a (Type' a) [Type' a]

--type Expr = Expr' BNFC'Position
--data Expr' a
--    = EVar a Ident
--    | ELitInt a Integer
--    | ELitTrue a
--    | ELitFalse a
--    | EApp a Ident [Expr' a]
--    | EString a String
--    | Neg a (Expr' a)
--    | Not a (Expr' a)
--    | EMul a (Expr' a) (MulOp' a) (Expr' a)
--    | EAdd a (Expr' a) (AddOp' a) (Expr' a)
--    | ERel a (Expr' a) (RelOp' a) (Expr' a)
--    | EAnd a (Expr' a) (Expr' a)
--    | EOr a (Expr' a) (Expr' a)

instance ProgramRunner Stmt where
  runCode (Empty _) = return None

  runCode (BStmt _ block) = do
    evalBasedOnReturn $ runCode block

  runCode (Decl pos sType [(NoInit _ ident)]) = do
    evalBasedOnReturn $ runCode $ Decl pos sType [(Init pos ident (defaultReturnValueForSimpleType sType))]

  runCode (Decl _ sType [(Init _ ident expr)]) = evalBasedOnReturn $ do
    x <- runCode expr
    modify $ putSimpleTypeValue ident x
    return None

  runCode (Decl pos sType decls) = evalBasedOnReturn $ do
    mapM_ (\decl -> runCode $ Decl pos sType [decl]) decls
    return None

instance ProgramRunner Expr where
  runCode (ELitInt _ x) = return $ Environment.Environment.Int x