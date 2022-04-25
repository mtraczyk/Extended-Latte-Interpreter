module ProgramInterpreter.ProgramInterpreterTypes where

import Environment.Environment
import Exception.RuntimeException
import Prelude
import Grammar.AbsLatte
import Control.Monad.Except
import Control.Monad.State

type ProgramEvaluator = Evaluator (Either SimpleType FunctionType)
type Evaluator a = StateT EvalEnvironment (ExceptT RuntimeException IO) a

class ProgramRunner a where
  runCode :: a -> ProgramEvaluator
