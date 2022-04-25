module ProgramInterpreter.Utils where

import ProgramInterpreter.ProgramInterpreterTypes
import Environment.Environment
import Exception.RuntimeException
import Prelude
import Grammar.AbsLatte
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M

-- Buildin functions
buildinFunctions = ["printInt", "printBool", "printString"]

maybeRunBuildinFunction :: Ident -> [Either SimpleType FunctionType] -> ProgramEvaluator -> ProgramEvaluator
maybeRunBuildinFunction (Ident name) args ste = do
  case name `elem` buildinFunctions of
    True -> do
      liftIO $ putStrLn (show args)
      return $ Left None
    False -> ste

isReturnDefined :: EvalEnvironment -> Bool
isReturnDefined env = let loc = getEnvLocation (Ident "return") (getVEnv env) in
  case M.lookup loc (getVStore env) of
    Just None -> False
    Just _ -> True
    Nothing -> False

evalBasedOnReturn :: ProgramEvaluator -> ProgramEvaluator
evalBasedOnReturn ste = do
  env <- get
  case isReturnDefined env of
    True -> return $ Left None
    False -> ste

defaultReturnValueForSimpleType :: Type -> Expr
defaultReturnValueForSimpleType (Grammar.AbsLatte.Int pos) = ELitInt pos 0
defaultReturnValueForSimpleType (Grammar.AbsLatte.Str pos) = EString pos ""
defaultReturnValueForSimpleType (Grammar.AbsLatte.Bool pos) = ELitFalse pos

runAndKeepEnv :: ProgramEvaluator -> ProgramEvaluator
runAndKeepEnv ste = do
  env <- get
  let vEnv = getVEnv env
  let fEnv = getFEnv env
  ste
  modify $ putVEnv vEnv
  modify $ putFEnv fEnv
  return $ Left None

applyFun :: (Integer -> Integer) -> SimpleType -> SimpleType
applyFun f (Environment.Environment.Int val) = Environment.Environment.Int $ f val