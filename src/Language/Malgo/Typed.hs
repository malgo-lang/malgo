module Language.Malgo.Typed where

import           Language.Malgo.Types

data Decl = DefVar Name Type Const
          | DefFun Name Type [(Name, Type)] Expr
          | ExVar Name Type
          | ExFun Name Type [(Name, Type)]
  deriving (Show, Eq)

data Const = Int Integer
           | Float Double
           | Bool Bool
           | Char Char
           | String String
           | Unit
           | CBinOp Op Const Const
  deriving (Show, Eq)

-- | 第一引数のTypeが式の型を表す
data Expr = Var Type Name
          | Const Type Const
          | Call Type Name [Expr]
          | Seq Type Expr Expr
          | Let Type Name Type Expr Expr
          | If Type Expr Expr Expr
          | BinOp Type Op Expr Expr
  deriving (Show, Eq)
