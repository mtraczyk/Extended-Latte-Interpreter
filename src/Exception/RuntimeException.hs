{-# LANGUAGE FlexibleInstances #-}
module Exception.RuntimeException where

import Grammar.AbsLatte

type RuntimeException = RuntimeException' BNFC'Position
data RuntimeException' a = UndefinedException a | DivideByZeroException a
  | UndefinedVariable a | UndefinedIdent a

instance Show RuntimeException where
  show (UndefinedException pos) =
    "RUNTIME EXCEPTION: Undefined exception! At " ++ showPos pos
  show (UndefinedVariable pos) =
    "RUNTIME EXCEPTION: Undefined variable! At " ++ showPos pos
  show (UndefinedIdent pos) =
    "RUNTIME EXCEPTION: Undefined identifier! At " ++ showPos pos
  show (DivideByZeroException pos) =
    "RUNTIME EXCEPTION: Divide by zero! At " ++ showPos pos

showPos :: BNFC'Position -> String
showPos (Just (line, column)) = "line: " ++ show line ++ ", " ++ "column: " ++ show column
showPos _ = "Error position not specified."
