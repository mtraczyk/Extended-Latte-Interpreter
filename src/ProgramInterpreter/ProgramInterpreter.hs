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

interpretProgram :: Program -> IO (Either RuntimeException (Either SimpleType FunctionType))
interpretProgram program = runExceptT $ evalStateT (runCode program) emptyEvalEnvironment

instance ProgramRunner Program where
  runCode (Program pos topDefs) = do
    modify $ putReturnValue None
    mapM_ runCode topDefs
    env <- get
    case isDefinedFunctionTypeValue (Ident "main") env of
      True -> do
        retValue <- runCode $ EApp pos (Ident "main") []
        return retValue
      False -> throwError $ MainFunctionUndefinedException pos

instance ProgramRunner TopDef where
  runCode (FnDef pos _ name args block) = do
    vEnv <- gets getVEnv
    fEnv <- gets getFEnv
    let fun = TFun args block vEnv fEnv
    case isNameQualified name of
      True -> modify $ putFunctionTypeValue name fun
      False -> throwError $ UnqualifiedIdException pos
    return $ Left None

  runCode (GloDecl pos sType decls) = do
    runCode $ Decl pos sType decls
    return $ Left None

instance ProgramRunner Block where
  runCode (Block _ stmts) = evalBasedOnReturn $ runAndKeepEnv $ do
    mapM_ (\stmt -> runCode stmt) stmts
    return $ Left None

instance ProgramRunner Stmt where
  runCode (Empty _) = return $ Left None

  runCode (BStmt _ block) =
    evalBasedOnReturn $ runAndKeepEnv $ runCode block

  runCode (Decl pos sType [(NoInit pos2 ident)]) =
    evalBasedOnReturn $ runCode $ Decl pos sType [(Init pos2 ident (defaultReturnValueForSimpleType sType))]

  runCode (Decl _ sType [(Init pos ident expr)]) = evalBasedOnReturn $ do
    x <- runCode expr
    case isNameQualified ident of
      True -> case x of
        Left val -> modify $ putSimpleTypeValue ident val
        Right val -> modify $ putFunctionTypeValue ident val
      False -> throwError $ UnqualifiedIdException pos
    return $ Left None

  runCode (Decl pos sType decls) = evalBasedOnReturn $ do
    mapM_ (\decl -> runCode $ Decl pos sType [decl]) decls
    return $ Left None

  runCode (Ass pos ident expr) = evalBasedOnReturn $ do
    env <- get
    x <- runCode expr
    case x of
      Left val -> evalBasedOnDefinedSimpleTypeValue ident env pos $ do
        modify $ updateSimpleTypeValue ident val
        return $ Left None
      Right val -> evalBasedOnDefinedFunctionTypeValue ident env pos $ do
        modify $ updateFunctionTypeValue ident val
        return $ Left None

  runCode (Incr pos ident) = evalBasedOnReturn $ do
    env <- get
    evalBasedOnDefinedSimpleTypeValue ident env pos $ do
      modify $ updateSimpleTypeValue ident (applyFun (\x -> x + 1) (getSimpleTypeValue ident env))
      return $ Left None

  runCode (Decr pos ident) = evalBasedOnReturn $ do
    env <- get
    evalBasedOnDefinedSimpleTypeValue ident env pos $ do
      modify $ updateSimpleTypeValue ident (applyFun (\x -> x - 1) (getSimpleTypeValue ident env))
      return $ Left None

  runCode (Ret pos expr) = evalBasedOnReturn $ do
    x <- runCode expr
    case x of
      Left val -> modify $ updateReturnValue val
      Right _ -> throwError $ WrongReturnTypeException pos
    return $ Left None

  runCode (VRet _) = evalBasedOnReturn $ do
    modify $ updateReturnValue VoidReturn
    return $ Left None

  runCode (Cond pos expr stmt) = evalBasedOnReturn $ runAndKeepEnv $ do
    x <- runCode expr
    case x of
      Left (Environment.Environment.Bool True) -> do
        runCode stmt
        return $ Left None
      Left (Environment.Environment.Bool False) -> return $ Left None
      Right _ -> throwError $ WrongTypeException pos

  runCode (CondElse pos expr ifStmt elseStmt) = evalBasedOnReturn $ runAndKeepEnv $ do
    x <- runCode expr
    case x of
      Left (Environment.Environment.Bool True) -> do
        runCode ifStmt
        return $ Left None
      Left (Environment.Environment.Bool False) -> do
        runCode elseStmt
        return $ Left None
      Right _ -> throwError $ WrongTypeException pos

  runCode while@(While pos expr stmt) = evalBasedOnReturn $ runAndKeepEnv $ do
    x <- runCode expr
    case x of
      Left (Environment.Environment.Bool True) -> evalBasedOnReturn $ runAndKeepEnv $ do
        runCode stmt
        runCode while
        return $ Left None
      Left (Environment.Environment.Bool False) -> return $ Left None
      Right _ -> throwError $ WrongTypeException pos

  runCode (STopDef _ def) = evalBasedOnReturn $ do
    runCode def
    return $ Left None

  runCode (SExp _ expr) = evalBasedOnReturn $ runCode expr

instance ProgramRunner Expr where
  runCode (EVar pos ident) = do
    env <- get
    case isDefinedSimpleTypeValue ident env of
      True -> return $ Left $ getSimpleTypeValue ident env
      False -> evalBasedOnDefinedFunctionTypeValue ident env pos $
        return $ Right $ getFunctionTypeValue ident env

  runCode (ELitInt _ x) = return $ Left $ Environment.Environment.Int x
  runCode (ELitTrue _) = return $ Left $ Environment.Environment.Bool True
  runCode (ELitFalse _) = return $ Left $ Environment.Environment.Bool False
  runCode (EString _ str) = return $ Left $ Environment.Environment.Str str

  runCode (EApp _ ident expressions) = do
    argsVal <- mapM runCode expressions
    maybeRunBuildinFunction ident argsVal $ do
      env <- get
      fun@(TFun args block funVEnv funFEnv) <- gets $ getFunctionTypeValue ident
      modify $ putVEnv funVEnv
      modify $ putFEnv funFEnv
      modify $ putFunctionTypeValue ident fun
      modify $ putReturnValue None
      modify (\environment -> (foldl (\acc ((Arg _ _ x), y) -> putTypeValue x y acc) environment (zip args argsVal)))
      runCode block
      funEnv' <- get
      let returnValue = getReturnValue funEnv'
      modify $ putVEnv (getVEnv env)
      modify $ putFEnv (getFEnv env)
      return $ Left returnValue
    where
      putTypeValue x y acc = case y of
        Left val -> putSimpleTypeValue x val acc
        Right val -> putFunctionTypeValue x val acc

  runCode (ELambda _ _ arguments block) = do
    env <- get
    return $ Right $ TFun arguments block (getVEnv env) (getFEnv env)

  runCode (Neg pos expr) = do
    x <- runCode expr
    case x of
      Left val -> return $ Left $ applyFun negate val
      Right _ -> throwError $ WrongTypeException pos

  runCode (Not pos expr) = do
    x <- runCode expr
    case x of
      Left (Environment.Environment.Bool True) -> return $ Left $ Environment.Environment.Bool False
      Left (Environment.Environment.Bool False) -> return $ Left $ Environment.Environment.Bool True
      Right _ -> throwError $ WrongTypeException pos

  runCode (EMul pos exprL op exprR) = do
    Left (Environment.Environment.Int x) <- runCode exprL
    Left (Environment.Environment.Int y) <- runCode exprR
    case op of
      Times _ -> return $ Left $ Environment.Environment.Int $ x * y
      Div pos -> do
        case y of
          0 ->  throwError $ DivideByZeroException pos
          _ ->  return $ Left $ Environment.Environment.Int $ x `div` y
      Mod pos ->
        case y of
          0 -> throwError $ DivideByZeroException pos
          _ -> return $ Left $ Environment.Environment.Int $ x `mod` y

  runCode (EAdd _ exprL op exprR) = do
    Left (Environment.Environment.Int x) <- runCode exprL
    Left (Environment.Environment.Int y) <- runCode exprR
    case op of
      Plus _ -> return $ Left $ Environment.Environment.Int $ x + y
      Minus _ -> return $ Left $ Environment.Environment.Int $ x - y

  runCode (EAnd _ exprL exprR) = do
    Left (Environment.Environment.Bool x) <- runCode exprL
    Left (Environment.Environment.Bool y) <- runCode exprR
    return $ Left $ Environment.Environment.Bool $ x && y

  runCode (EOr _ exprL exprR) = do
    Left (Environment.Environment.Bool x) <- runCode exprL
    Left (Environment.Environment.Bool y) <- runCode exprR
    return $ Left $ Environment.Environment.Bool $ x || y

  runCode (ERel _ exprL op exprR) = do
    Left (Environment.Environment.Int x) <- runCode exprL
    Left (Environment.Environment.Int y) <- runCode exprR
    case op of
      LTH _ -> return $ Left $ Environment.Environment.Bool $ x < y
      LE _ -> return $ Left $ Environment.Environment.Bool $ x <= y
      GTH _ -> return $ Left $ Environment.Environment.Bool $ x > y
      GE _ -> return $ Left $ Environment.Environment.Bool $ x >= y
      EQU _ -> return $ Left $ Environment.Environment.Bool $ x == y
      NE _ -> return $ Left $ Environment.Environment.Bool $ x /= y
