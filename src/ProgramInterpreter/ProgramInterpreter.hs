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

--    | Break a
--    | Continue a

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

  runCode (CondElse _ expr ifStmt elseStmt) = evalBasedOnReturn $ runAndKeepEnv $ do
    x <- runCode expr
    case x of
      Environment.Environment.Bool True -> do
        runCode ifStmt
        return None
      Environment.Environment.Bool False -> do
        runCode elseStmt
        return None

  runCode while@(While _ expr stmt) = evalBasedOnReturn $ runAndKeepEnv $ do
    x <- runCode expr
    case x of
      Environment.Environment.Bool True -> evalBasedOnReturn $ runAndKeepEnv $ do
        runCode while
        return None
      Environment.Environment.Bool False -> return None

  runCode (SExp _ expr) = evalBasedOnReturn $ runCode expr


--    | EApp a Ident [Expr' a]

instance ProgramRunner Expr where
  runCode (EVar _ ident) = do
    env <- get
    return $ getSimpleTypeValue ident env

  runCode (ELitInt _ x) = return $ Environment.Environment.Int x
  runCode (ELitTrue _) = return $ Environment.Environment.Bool True
  runCode (ELitFalse _) = return $ Environment.Environment.Bool False
  runCode (EString _ str) = return $ Environment.Environment.Str str

  runCode (EApp _ ident expressions) = do
    argsVal <- mapM runCode expressions
    maybeRunBuildinFunction ident argsVal $ do
      env <- get
      case getFunctionTypeValue ident env of
        TFun args block funEnv -> return None
        TVoid -> return None

  runCode (Neg _ expr) = do
    x <- runCode expr
    return $ applyFun negate x

  runCode (Not _ expr) = do
    x <- runCode expr
    case x of
      Environment.Environment.Bool True -> return $ Environment.Environment.Bool False
      Environment.Environment.Bool False -> return $ Environment.Environment.Bool True

  runCode (EMul _ exprL op exprR) = do
    (Environment.Environment.Int x) <- runCode exprL
    (Environment.Environment.Int y) <- runCode exprR
    case op of
      Times _ -> return $ Environment.Environment.Int $ x * y
      Div _ -> return $ Environment.Environment.Int $ x `div` y
      Mod _ -> return $ Environment.Environment.Int $ x `mod` y

  runCode (EAdd _ exprL op exprR) = do
    (Environment.Environment.Int x) <- runCode exprL
    (Environment.Environment.Int y) <- runCode exprR
    case op of
      Plus _ -> return $ Environment.Environment.Int $ x + y
      Minus _ -> return $ Environment.Environment.Int $ x - y

  runCode (EAnd _ exprL exprR) = do
    (Environment.Environment.Bool x) <- runCode exprL
    (Environment.Environment.Bool y) <- runCode exprR
    return $ Environment.Environment.Bool $ x && y

  runCode (EOr _ exprL exprR) = do
    (Environment.Environment.Bool x) <- runCode exprL
    (Environment.Environment.Bool y) <- runCode exprR
    return $ Environment.Environment.Bool $ x || y

  runCode (ERel _ exprL op exprR) = do
    (Environment.Environment.Bool x) <- runCode exprL
    (Environment.Environment.Bool y) <- runCode exprR
    case op of
      LTH _ -> return $ Environment.Environment.Bool $ x < y
      LE _ -> return $ Environment.Environment.Bool $ x <= y
      GTH _ -> return $ Environment.Environment.Bool $ x > y
      GE _ -> return $ Environment.Environment.Bool $ x >= y
      EQU _ -> return $ Environment.Environment.Bool $ x == y
      NE _ -> return $ Environment.Environment.Bool $ x /= y