{-# LANGUAGE RecordWildCards #-}
module Environment.Environment where

import qualified Data.Map as M
import Data.Maybe
import Prelude
import Grammar.AbsLatte

type Location = Int
newtype Env = Env {env :: M.Map Ident Location} deriving (Eq, Show)

data EvalEnvironment = EvalEnvironment {vEnv :: Env, fEnv :: Env, store :: Store} deriving (Eq, Show)

data Store = Store {vStore :: M.Map Location SimpleType,
  fStore :: M.Map Location FunctionType, vAlloc :: Int, fAlloc :: Int} deriving (Eq, Show)

data FunctionType = TFun [Arg] Block Env Env deriving (Eq, Show)
data SimpleType = Int Integer | Str String | Bool Bool | VoidReturn | None deriving (Eq, Show)

getStoreSimpleType :: Location -> Store -> SimpleType
getStoreSimpleType loc Store{..} = fromJust $ M.lookup loc vStore

updateStoreSimpleType :: Location -> SimpleType -> Store -> Store
updateStoreSimpleType loc val Store{..} = Store {vStore = M.insert loc val vStore,
  fStore = fStore, vAlloc = vAlloc, fAlloc = fAlloc}

putStoreSimpleType :: SimpleType -> Store -> (Location, Store)
putStoreSimpleType val store = (vAlloc', store') where
  vAlloc' = (vAlloc store) + 1
  store' = Store {vStore = M.insert vAlloc' val (vStore store),
    fStore = fStore store, vAlloc = vAlloc', fAlloc = fAlloc store}

getStoreFunctionType :: Location -> Store -> FunctionType
getStoreFunctionType loc Store{..} = fromJust $ M.lookup loc fStore

updateStoreFunctionType :: Location -> FunctionType -> Store -> Store
updateStoreFunctionType location fun Store{..} = Store {vStore = vStore,
  fStore = M.insert location fun fStore, vAlloc = vAlloc, fAlloc = fAlloc}

putStoreFunctionType :: FunctionType -> Store -> (Location, Store)
putStoreFunctionType fun store = (fAlloc', store') where
  fAlloc' = (fAlloc store) + 1
  store' = Store {vStore = vStore store, fStore = M.insert fAlloc' fun (fStore store),
    vAlloc = vAlloc store, fAlloc = fAlloc'}

getEnvLocation :: Ident -> Env -> Location
getEnvLocation ident Env{..} = fromJust $ M.lookup ident env

putEnvLocation :: Ident -> Location -> Env -> Env
putEnvLocation ident loc Env{..} = Env {env = M.insert ident loc env}

getVEnv :: EvalEnvironment -> Env
getVEnv EvalEnvironment{..} = vEnv

putVEnv :: Env -> EvalEnvironment -> EvalEnvironment
putVEnv vEnv' EvalEnvironment{..} = EvalEnvironment {vEnv = vEnv', fEnv = fEnv, store = store}

getFEnv :: EvalEnvironment -> Env
getFEnv EvalEnvironment{..} = fEnv

putFEnv :: Env -> EvalEnvironment -> EvalEnvironment
putFEnv fEnv' EvalEnvironment{..} = EvalEnvironment {vEnv = vEnv, fEnv = fEnv', store = store}

getVStore :: EvalEnvironment -> (M.Map Location SimpleType)
getVStore EvalEnvironment{..} = vStore store

getFStore :: EvalEnvironment -> (M.Map Location FunctionType)
getFStore EvalEnvironment{..} = fStore store

isDefinedSimpleTypeValue :: Ident -> EvalEnvironment -> Bool
isDefinedSimpleTypeValue ident EvalEnvironment{..} = case M.lookup ident (env vEnv) of
  Just _ -> True
  Nothing -> False

getSimpleTypeValue :: Ident -> EvalEnvironment -> SimpleType
getSimpleTypeValue ident env = getStoreSimpleType (getEnvLocation ident (getVEnv env)) (store env)

updateSimpleTypeValue :: Ident -> SimpleType -> EvalEnvironment -> EvalEnvironment
updateSimpleTypeValue ident val env = EvalEnvironment {vEnv = vEnv env, fEnv = fEnv env, store = store'}
  where
    store' = updateStoreSimpleType (getEnvLocation ident (getVEnv env)) val (store env)

putSimpleTypeValue :: Ident -> SimpleType -> EvalEnvironment -> EvalEnvironment
putSimpleTypeValue ident val env = EvalEnvironment {vEnv = vEnv', fEnv = fEnv env, store = store'}
  where
    (loc', store') = putStoreSimpleType val (store env)
    vEnv' = putEnvLocation ident loc' (vEnv env)

isDefinedFunctionTypeValue :: Ident -> EvalEnvironment -> Bool
isDefinedFunctionTypeValue ident EvalEnvironment{..} = case M.lookup ident (env fEnv) of
  Just _ -> True
  Nothing -> False

getFunctionTypeValue :: Ident -> EvalEnvironment -> FunctionType
getFunctionTypeValue ident env = getStoreFunctionType (getEnvLocation ident (getFEnv env)) (store env)

updateFunctionTypeValue :: Ident -> FunctionType -> EvalEnvironment -> EvalEnvironment
updateFunctionTypeValue ident val env = EvalEnvironment {vEnv = vEnv env, fEnv = fEnv env, store = store'}
  where
    store' = updateStoreFunctionType (getEnvLocation ident (getFEnv env)) val (store env)

putFunctionTypeValue :: Ident -> FunctionType -> EvalEnvironment -> EvalEnvironment
putFunctionTypeValue ident val env = EvalEnvironment {vEnv = vEnv env, fEnv = fEnv', store = store'}
  where
    (loc', store') = putStoreFunctionType val (store env)
    fEnv' = putEnvLocation ident loc' (fEnv env)

getReturnValue :: EvalEnvironment -> SimpleType
getReturnValue env = getSimpleTypeValue (Ident "return") env

updateReturnValue :: SimpleType -> EvalEnvironment -> EvalEnvironment
updateReturnValue val env = updateSimpleTypeValue (Ident "return") val env

putReturnValue :: SimpleType -> EvalEnvironment -> EvalEnvironment
putReturnValue val env = putSimpleTypeValue (Ident "return") val env

emptyStore = Store {vStore = M.empty, fStore = M.empty, vAlloc = 0, fAlloc = 0}
emptyEvalEnvironment = EvalEnvironment {vEnv = Env {env = M.empty}, fEnv = Env {env = M.empty}, store = emptyStore}




