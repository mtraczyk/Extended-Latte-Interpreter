{-# LANGUAGE FlexibleInstances #-}
module Exception.RuntimeException where

import Grammar.AbsLatte

type RuntimeException = RuntimeException' BNFC'Position
data RuntimeException' a = UndefinedException a | DivideByZeroException a | UndefinedFunctionException a
  | UndefinedVariableException a | UndefinedIdentException a | WrongReturnTypeException a | WrongTypeException a
  | MainFunctionUndefinedException a | UnqualifiedIdException a

instance Show RuntimeException where
  show (UndefinedException pos) =
    "At " ++ showPos pos ++ ". RUNTIME EXCEPTION: Undefined exception!"
  show (UndefinedFunctionException pos) =
    "At " ++ showPos pos ++ ". RUNTIME EXCEPTION: Undefined function!"
  show (UndefinedVariableException pos) =
    "At " ++ showPos pos ++ ". RUNTIME EXCEPTION: Undefined variable!"
  show (UndefinedIdentException pos) =
    "At " ++ showPos pos ++ ". RUNTIME EXCEPTION: Undefined identifier!"
  show (WrongReturnTypeException pos) =
    "At " ++ showPos pos ++ ". RUNTIME EXCEPTION: Wrong return type!"
  show (WrongTypeException pos) =
    "At " ++ showPos pos ++ ". RUNTIME EXCEPTION: Wrong return type!"
  show (MainFunctionUndefinedException pos) =
    "At " ++ showPos pos ++ ". RUNTIME EXCEPTION: Main function undefined!"
  show (DivideByZeroException pos) =
    "At " ++ showPos pos ++ ". RUNTIME EXCEPTION: Division by zero!"
  show (UnqualifiedIdException pos) =
    "At " ++ showPos pos ++ ". RUNTIME EXCEPTION: Unqualified ID!"

showPos :: BNFC'Position -> String
showPos (Just (line, column)) = "line: " ++ show line ++ ", " ++ "column: " ++ show column
showPos _ = "Error position not specified."
