{-# LANGUAGE FlexibleInstances #-}
module Exception.RuntimeException where

import Syntax.AbsLatte

type RuntimeException = RuntimeException' BNFC'Position
data RuntimeException' a = UndefinedException a | DivideByZeroException a | UndefinedBuildinFunction a

instance Show RuntimeException where
  show (UndefinedException pos) =
    "RUNTIME EXCEPTION: Undefined exception! At " ++ showPosition pos
  show (UndefinedBuildinFunction pos) =
    "RUNTIME EXCEPTION: Undefined buildin function! At " ++ showPosition pos
  show (DivideByZeroException pos) =
    "RUNTIME EXCEPTION: Divide by zero! At " ++ showPosition pos

showPosition :: BNFC'Position -> String
showPosition (Just (line, column)) = "line: " ++ show line ++ ", " ++ "column: " ++ show column
showPosition _ = "Not specified"
