{-# LANGUAGE RecordWildCards #-}
module Evaluator.Environment where

import qualified Data.Map as M
import Data.Maybe
import Prelude
import Syntax.AbsLatte

type Location = Int
newtype Env = Env {env :: M.Map Ident Location}

data EvalEnvironment = EvalEnvironment {vEnv :: Env, fEnv :: Env, store :: Store}

data Store = Store {vStore :: M.Map Location SimpleType,
  fStore :: M.Map Location FunctionType, vAlloc :: Int, fAlloc :: Int}

data SimpleType = Int Integer | Str String | Bool Bool deriving (Eq, Show)
data FunctionType = Fun [Arg] Block Env | Void

getStoreSimpleType :: Location -> Store -> SimpleType
getStoreSimpleType loc Store{..} = fromJust $ M.lookup loc vStore

updateStoreValue :: Location -> SimpleType -> Store -> Store
updateStoreValue loc val Store{..} = Store {vStore = M.insert loc val vStore,
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





