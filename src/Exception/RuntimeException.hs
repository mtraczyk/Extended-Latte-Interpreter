{-# LANGUAGE FlexibleInstances #-}
module Exception.RuntimeException where

import Grammar.AbsLatte

type RuntimeException = RuntimeException' BNFC'Position
data RuntimeException' a = UndefinedException a | DivideByZeroException a | UndefinedFunction a
  | UndefinedVariable a | UndefinedIdent a | WrongReturnTypeError a | WrongTypeError a
  | MainFunctionUndefinedError a

instance Show RuntimeException where
  show (UndefinedException pos) =
    "At " ++ showPos pos ++ ". RUNTIME EXCEPTION: Undefined exception!"
  show (UndefinedFunction pos) =
    "At " ++ showPos pos ++ ". RUNTIME EXCEPTION: Undefined function!"
  show (UndefinedVariable pos) =
    "At " ++ showPos pos ++ ". RUNTIME EXCEPTION: Undefined variable!"
  show (UndefinedIdent pos) =
    "At " ++ showPos pos ++ ". RUNTIME EXCEPTION: Undefined identifier!"
  show (WrongReturnTypeError pos) =
    "At " ++ showPos pos ++ ". RUNTIME EXCEPTION: Wrong return type!"
  show (WrongTypeError pos) =
    "At " ++ showPos pos ++ ". RUNTIME EXCEPTION: Wrong return type!"
  show (MainFunctionUndefinedError pos) =
    "At " ++ showPos pos ++ ". RUNTIME EXCEPTION: Main function undefined!"
  show (DivideByZeroException pos) =
    "At " ++ showPos pos ++ ". RUNTIME EXCEPTION: Divide by zero!"

showPos :: BNFC'Position -> String
showPos (Just (line, column)) = "line: " ++ show line ++ ", " ++ "column: " ++ show column
showPos _ = "Error position not specified."
