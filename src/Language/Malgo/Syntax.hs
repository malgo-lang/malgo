{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Malgo.Syntax where

type Name = String

data Decl = Def Name Type Expr
          | Defun Name Type [(Name, Type)] Expr
  deriving Show

data Expr = Var Name
          | Int Integer
          | Float Double
          | Bool Bool
          | Char Char
          | String String
          | Call Name [Expr]
          | Block [Expr]
          | If Expr Expr Expr
  deriving Show

data Type = IntTy
          | FloatTy
          | BoolTy
          | CharTy
          | StringTy
          | FunTy Type [Type]
  deriving Show
