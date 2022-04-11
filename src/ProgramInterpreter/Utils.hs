module ProgramInterpreter.Utils where

import ProgramInterpreter.ProgramInterpreterTypes
import Environment.Environment
import Exception.RuntimeException
import Prelude
import Grammar.AbsLatte
import Control.Monad.Except
import Control.Monad.State

-- Buildin functions
buildinFunctions = ["printInt", "printBool", "printString"]

runBuildinFunction :: Ident -> [SimpleType] -> SimpleTypeEvaluator
runBuildinFunction (Ident name) val = case name `elem` buildinFunctions of
  True -> do
    liftIO $ putStrLn (show val)
    return None
  False -> throwError $ UndefinedBuildinFunction Nothing
