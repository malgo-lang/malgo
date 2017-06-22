{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Syntax (Name, Type, Expr(..)) where

import           Control.Monad.State (evalState)
import           Symbol

type Name = Symbol
data Type = IntT
          | BoolT
          | FunT [Type] Type
  deriving Show

data TypedVar = (:-:) Name Type
  deriving Show

data Expr = Nil
          | Int Int
          | Bool Bool
          | Func { params :: [TypedVar]
                 , body   :: [Expr]
                 }
          | Def TypedVar Expr
          | Call Name [Expr]
          | Var Name
          | If Expr Expr Expr
          | Let TypedVar Expr [Expr]
  deriving Show

defineAdd :: Expr
defineAdd = (`evalState` empty) $  do
  add <- symbol "add"
  plus <- symbol "+"
  x <- symbol "x"
  y <- symbol "y"
  return $ Def (add :-: FunT [IntT, IntT] IntT) Func { params = [ x :-: IntT
                                                                , y :-: IntT
                                                                ]
                                                     , body = [Call plus [Var x, Var y]]
                                                     }
