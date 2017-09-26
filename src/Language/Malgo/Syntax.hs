module Language.Malgo.Syntax where

data Name = Sym String
          | Id Int
  deriving (Eq, Show)

data Decl = Def Name Type Expr
          | Defun Name Type [(Name, Type)] Expr
  deriving (Eq, Show)

data Expr = Var Name
          | Int Integer
          | Float Double
          | Bool Bool
          | Char Char
          | String String
          | Unit
          | Call Name [Expr]
          | Seq Expr Expr
          | If Expr Expr Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Eq Expr Expr
          | Lt Expr Expr
          | Gt Expr Expr
          | And Expr Expr
          | Or Expr Expr
  deriving (Eq, Show)

data Type = IntTy
          | FloatTy
          | BoolTy
          | CharTy
          | StringTy
          | UnitTy
          | FunTy Type [Type]
  deriving (Eq, Show)
