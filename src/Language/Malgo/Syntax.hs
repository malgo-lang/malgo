{-# LANGUAGE StrictData #-}

module Language.Malgo.Syntax where

import           Control.Lens    ()
import           Text.Parsec.Pos

-- | 'Name' is used like symbols on LISP
type Name = String

mkName :: String -> Name
mkName = id

type Pos = SourcePos

{-|
'Def' -> def <name>:<type> = <expr>
'Defun' -> def <name>(<param1>:<type1>, <param2>:<type2>, ...):<result type> = <expr>
'ExDef' -> extern <name>:<type>
'ExDefun' -> extern <name>(<param1>:<type1>, <param2>:<type2>, ...):<result type>
-}
data Decl = Def Pos Name Type Expr
          | Defun Pos Name Type [(Name, Type)] Expr
          | ExDef Pos Name Type
          | ExDefun Pos Name Type [(Name, Type)]
  deriving (Eq, Show)

data Expr = Var Pos Name
          | Int Pos Int
          | Float Pos Double
          | Bool Pos Bool
          | Char Pos Char
          | String Pos String
          | Unit Pos
          | Call Pos Name [Expr]
          | Seq Pos Expr Expr
          | Let Pos Name Type Expr
          | If Pos Expr Expr Expr
          | BinOp Pos Op Expr Expr
  deriving (Eq, Show)

data Op = Add | Sub | Mul | Div | Eq | Neq | Lt | Gt | Le | Ge | And | Or
  deriving (Eq, Show)

data Type = IntTy
          | FloatTy
          | BoolTy
          | CharTy
          | StringTy
          | UnitTy
          | FunTy Type [Type]
  deriving (Eq, Show)
