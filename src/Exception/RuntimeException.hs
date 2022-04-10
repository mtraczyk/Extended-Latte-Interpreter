{-# LANGUAGE FlexibleInstances #-}
module Exception.RuntimeException where

import Grammar.AbsLatte

type RuntimeException = RuntimeException' BNFC'Position
data RuntimeException' a = UndefinedException a | DivideByZeroException a | UndefinedBuildinFunction a

instance Show RuntimeException where
  show (UndefinedException pos) =
    "RUNTIME EXCEPTION: Undefined exception! At " ++ showPos pos
  show (UndefinedBuildinFunction pos) =
    "RUNTIME EXCEPTION: Undefined buildin function! At " ++ showPos pos
  show (DivideByZeroException pos) =
    "RUNTIME EXCEPTION: Divide by zero! At " ++ showPos pos

showPos :: BNFC'Position -> String
showPos (Just (line, column)) = "line: " ++ show line ++ ", " ++ "column: " ++ show column
showPos _ = "Error position not specified."
