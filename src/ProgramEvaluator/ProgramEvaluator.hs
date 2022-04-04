{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
module ProgramEvaluator.ProgramEvaluator where

import Environment.Environment
import Exception.RuntimeException
import Prelude
import Syntax.AbsLatte
import Control.Monad.Except
import Control.Monad.State

type SimpleTypeEvaluator = Evaluator SimpleType
type Evaluator a = StateT EvalEnvironment (ExceptT RuntimeException IO) a

evalProgram :: Program -> IO (Either RuntimeException SimpleType)
evalProgram program = runExceptT $ evalStateT (eval program) emptyEvalEnvironment

class ProgramEvaluator a where
  eval :: a -> SimpleTypeEvaluator

-- Buildin functions
buildinFunctions = ["printInt", "printBool", "printString"]

evalBuildinFunction :: Ident -> [SimpleType] -> SimpleTypeEvaluator
evalBuildinFunction (Ident name) val = case name `elem` buildinFunctions of
  True -> do
    liftIO $ putStrLn (show val)
    return None
  False -> throwError $ UndefinedBuildinFunction Nothing

instance ProgramEvaluator Program where
  eval (Program pos topDefs) = do
    evalBuildinFunction (Ident "printInt") [(Environment.Environment.Int 4), (Environment.Environment.Bool True)]
    return (Environment.Environment.Int 3)
--    mapM_ eval topDefs
--    eval $ EApp position (Ident "main") []


