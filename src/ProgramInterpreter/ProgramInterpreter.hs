{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
module ProgramInterpreter.ProgramInterpreter where

import Environment.Environment
import Exception.RuntimeException
import Prelude
import Grammar.AbsLatte
import Control.Monad.Except
import Control.Monad.State

type SimpleTypeEvaluator = Evaluator SimpleType
type Evaluator a = StateT EvalEnvironment (ExceptT RuntimeException IO) a

interpretProgram :: Program -> IO (Either RuntimeException SimpleType)
interpretProgram program = runExceptT $ evalStateT (runCode program) emptyEvalEnvironment

class ProgramRunner a where
  runCode :: a -> SimpleTypeEvaluator

-- Buildin functions
buildinFunctions = ["printInt", "printBool", "printString"]

evalBuildinFunction :: Ident -> [SimpleType] -> SimpleTypeEvaluator
evalBuildinFunction (Ident name) val = case name `elem` buildinFunctions of
  True -> do
    liftIO $ putStrLn (show val)
    return None
  False -> throwError $ UndefinedBuildinFunction Nothing

instance ProgramRunner Program where
  runCode (Program pos topDefs) = do
    mapM_ runCode topDefs
    -- eval main function
    return None

--    evalBuildinFunction (Ident "printInt") [(Environment.Environment.Int 4), (Environment.Environment.Bool True)]
--    return (Environment.Environment.Int 3)
--    mapM_ eval topDefs
--    eval $ EApp position (Ident "main") []

instance ProgramRunner TopDef where
  runCode (FnDef _ _ name args block) = do
    env <- get
    let fEnv = getFEnv env
    let fun = TFun args block fEnv
    modify $ putFunctionTypeValue name fun
    return None

