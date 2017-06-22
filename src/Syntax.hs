{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Syntax (Name, Type, Func(..), Expr(..)) where

type Name = String
type Type = String

data TypedVar = (:-:) Name Type
  deriving Show

data Func = Func { name       :: Name
                 , returnType :: Type
                 , params     :: [TypedVar]
                 , body       :: [Expr]
                 }
  deriving Show

data Expr = Nil
          | Int Int
          | Bool Bool
          | Defn Func
          | Def TypedVar Expr
          | Call Name [Expr]
          | Var Name
          | If Expr Expr Expr
          | Let TypedVar Expr [Expr]
  deriving Show

defineAdd =
  Defn $ Func { name = "add"
              , returnType = "int"
              , params = ["x" :-: "int", "y" :-: "int"]
              , body = [Call "+" [Var "x", Var "y"]]
              }
