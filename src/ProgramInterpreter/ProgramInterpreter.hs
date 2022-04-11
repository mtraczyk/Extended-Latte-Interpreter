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
  runCode (Block _ stmts) = evalBasedOnReturn $ runAndKeepEnv $ do
    mapM_ (\stmt -> runCode stmt) stmts
    return None

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

--    | Ass a Ident (Expr' a) +
--    | Incr a Ident +
--    | Decr a Ident +
--    | Ret a (Expr' a) +
--    | VRet a +
--    | Cond a (Expr' a) (Stmt' a) +
--    | CondElse a (Expr' a) (Stmt' a) (Stmt' a) +
--    | While a (Expr' a) (Stmt' a)
--    | Break a
--    | Continue a
--    | SExp a (Expr' a) +

instance ProgramRunner Stmt where
  runCode (Empty _) = return None

  runCode (BStmt _ block) =
    evalBasedOnReturn $ runAndKeepEnv $ runCode block

  runCode (Decl pos sType [(NoInit _ ident)]) =
    evalBasedOnReturn $ runCode $ Decl pos sType [(Init pos ident (defaultReturnValueForSimpleType sType))]

  runCode (Decl _ sType [(Init _ ident expr)]) = evalBasedOnReturn $ do
    x <- runCode expr
    modify $ putSimpleTypeValue ident x
    return None

  runCode (Decl pos sType decls) = evalBasedOnReturn $ do
    mapM_ (\decl -> runCode $ Decl pos sType [decl]) decls
    return None

  runCode (Ass _ ident expr) = evalBasedOnReturn $ do
    x <- runCode expr
    modify $ updateSimpleTypeValue ident x
    return None

  runCode (Incr _ ident) = evalBasedOnReturn $ do
    env <- get
    modify $ updateSimpleTypeValue ident (applyFun (\x -> x + 1) (getSimpleTypeValue ident env))
    return None

  runCode (Decr _ ident) = evalBasedOnReturn $ do
    env <- get
    modify $ updateSimpleTypeValue ident (applyFun (\x -> x - 1) (getSimpleTypeValue ident env))
    return None

  runCode (Ret _ expr) = evalBasedOnReturn $ do
    x <- runCode expr
    modify $ updateReturnValue x
    return None

  runCode (VRet _) = evalBasedOnReturn $ do
    modify $ updateReturnValue VoidReturn
    return None

  runCode (Cond _ expr stmt) = evalBasedOnReturn $ runAndKeepEnv $ do
    x <- runCode expr
    case x of
      Environment.Environment.Bool True -> do
        runCode stmt
        return None
      Environment.Environment.Bool False -> return None

    return None

  runCode (CondElse _ expr ifStmt elseStmt) = evalBasedOnReturn $ runAndKeepEnv $ do
    x <- runCode expr
    case x of
      Environment.Environment.Bool True -> do
        runCode ifStmt
        return None
      Environment.Environment.Bool False -> do
        runCode elseStmt
        return None

    return None

  runCode while@(While _ expr stmt) = evalBasedOnReturn $ runAndKeepEnv $ do
    x <- runCode expr
    case x of
      Environment.Environment.Bool True -> evalBasedOnReturn $ runAndKeepEnv $ do
        runCode while
        return None
      Environment.Environment.Bool False -> return None

    return None

  runCode (SExp _ expr) = evalBasedOnReturn $ runCode expr

instance ProgramRunner Expr where
  runCode (ELitInt _ x) = return $ Environment.Environment.Int x