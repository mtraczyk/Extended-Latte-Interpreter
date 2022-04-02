-- File generated by the BNF Converter (bnfc 2.9.4).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Exception.SkelLatte where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified Syntax.AbsLatte

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: Syntax.AbsLatte.Ident -> Result
transIdent x = case x of
  Syntax.AbsLatte.Ident string -> failure x

transProgram :: Show a => Syntax.AbsLatte.Program' a -> Result
transProgram x = case x of
  Syntax.AbsLatte.Program _ topdefs -> failure x

transTopDef :: Show a => Syntax.AbsLatte.TopDef' a -> Result
transTopDef x = case x of
  Syntax.AbsLatte.FnDef _ type_ ident args block -> failure x

transArg :: Show a => Syntax.AbsLatte.Arg' a -> Result
transArg x = case x of
  Syntax.AbsLatte.Arg _ type_ ident -> failure x

transBlock :: Show a => Syntax.AbsLatte.Block' a -> Result
transBlock x = case x of
  Syntax.AbsLatte.Block _ stmts -> failure x

transStmt :: Show a => Syntax.AbsLatte.Stmt' a -> Result
transStmt x = case x of
  Syntax.AbsLatte.Empty _ -> failure x
  Syntax.AbsLatte.BStmt _ block -> failure x
  Syntax.AbsLatte.Decl _ type_ items -> failure x
  Syntax.AbsLatte.Ass _ ident expr -> failure x
  Syntax.AbsLatte.Incr _ ident -> failure x
  Syntax.AbsLatte.Decr _ ident -> failure x
  Syntax.AbsLatte.Ret _ expr -> failure x
  Syntax.AbsLatte.VRet _ -> failure x
  Syntax.AbsLatte.Cond _ expr stmt -> failure x
  Syntax.AbsLatte.CondElse _ expr stmt1 stmt2 -> failure x
  Syntax.AbsLatte.While _ expr stmt -> failure x
  Syntax.AbsLatte.SExp _ expr -> failure x

transItem :: Show a => Syntax.AbsLatte.Item' a -> Result
transItem x = case x of
  Syntax.AbsLatte.NoInit _ ident -> failure x
  Syntax.AbsLatte.Init _ ident expr -> failure x

transType :: Show a => Syntax.AbsLatte.Type' a -> Result
transType x = case x of
  Syntax.AbsLatte.Int _ -> failure x
  Syntax.AbsLatte.Str _ -> failure x
  Syntax.AbsLatte.Bool _ -> failure x
  Syntax.AbsLatte.Void _ -> failure x
  Syntax.AbsLatte.Fun _ type_ types -> failure x

transExpr :: Show a => Syntax.AbsLatte.Expr' a -> Result
transExpr x = case x of
  Syntax.AbsLatte.EVar _ ident -> failure x
  Syntax.AbsLatte.ELitInt _ integer -> failure x
  Syntax.AbsLatte.ELitTrue _ -> failure x
  Syntax.AbsLatte.ELitFalse _ -> failure x
  Syntax.AbsLatte.EApp _ ident exprs -> failure x
  Syntax.AbsLatte.EString _ string -> failure x
  Syntax.AbsLatte.Neg _ expr -> failure x
  Syntax.AbsLatte.Not _ expr -> failure x
  Syntax.AbsLatte.EMul _ expr1 mulop expr2 -> failure x
  Syntax.AbsLatte.EAdd _ expr1 addop expr2 -> failure x
  Syntax.AbsLatte.ERel _ expr1 relop expr2 -> failure x
  Syntax.AbsLatte.EAnd _ expr1 expr2 -> failure x
  Syntax.AbsLatte.EOr _ expr1 expr2 -> failure x

transAddOp :: Show a => Syntax.AbsLatte.AddOp' a -> Result
transAddOp x = case x of
  Syntax.AbsLatte.Plus _ -> failure x
  Syntax.AbsLatte.Minus _ -> failure x

transMulOp :: Show a => Syntax.AbsLatte.MulOp' a -> Result
transMulOp x = case x of
  Syntax.AbsLatte.Times _ -> failure x
  Syntax.AbsLatte.Div _ -> failure x
  Syntax.AbsLatte.Mod _ -> failure x

transRelOp :: Show a => Syntax.AbsLatte.RelOp' a -> Result
transRelOp x = case x of
  Syntax.AbsLatte.LTH _ -> failure x
  Syntax.AbsLatte.LE _ -> failure x
  Syntax.AbsLatte.GTH _ -> failure x
  Syntax.AbsLatte.GE _ -> failure x
  Syntax.AbsLatte.EQU _ -> failure x
  Syntax.AbsLatte.NE _ -> failure x
