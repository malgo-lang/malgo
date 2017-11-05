{-# LANGUAGE StrictData #-}

module Language.Malgo.Syntax where

import           Control.Lens    ()
import           Text.Parsec.Pos

type Name = String

mkName :: String -> Name
mkName = id

type Pos = SourcePos

-- | 宣言の構文木
data Decl = Def Pos Name Type Expr
          | Defun Pos Name Type [(Name, Type)] Expr
          | ExDef Pos Name Type
          | ExDefun Pos Name Type [(Name, Type)]
  deriving (Eq, Show)

-- | 式の構文木
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

-- | 中置演算子の種類を表すタグ
data Op = Add | Sub | Mul | Div | Eq | Neq | Lt | Gt | Le | Ge | And | Or
  deriving (Eq, Show)

-- | Malgoの組み込みデータ型
data Type = IntTy
          | FloatTy
          | BoolTy
          | CharTy
          | StringTy
          | UnitTy
          | FunTy Type [Type]
  deriving (Eq, Show)
