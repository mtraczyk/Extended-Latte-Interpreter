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

instance ProgramRunner Stmt where
  runCode (Empty _) = return None

