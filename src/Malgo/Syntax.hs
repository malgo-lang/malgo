module Malgo.Syntax
  ( Expr (..),
    Literal (..),
  )
where

import Malgo.Prelude

data Expr a
  = Var a
  | Lit Literal
  | App (Expr a) [Expr a]
  deriving stock (Functor, Foldable, Traversable)

newtype Literal = Int Integer