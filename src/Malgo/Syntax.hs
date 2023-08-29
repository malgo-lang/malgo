module Malgo.Syntax
  ( Expr (..),
    Literal (..),
    Clause (..),
    Pat (..),
    exprToPat,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString)
import Malgo.Prelude

data Expr a
  = Var a
  | Lit Literal
  | App (Expr a) (NonEmpty (Expr a))
  | Codata (NonEmpty (Clause a))
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

newtype Literal = Int Integer
  deriving stock (Eq, Show)

data Clause a = Clause {pattern :: Pat a, body :: Expr a}
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

data Pat a = PThis | PVar a | PLit Literal | PApp (Pat a) (NonEmpty (Pat a))
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

exprToPat :: (IsString a, Eq a) => Expr a -> Maybe (Pat a)
exprToPat (Var a)
  | a == "#" = Just PThis
  | otherwise = Just $ PVar a
exprToPat (Lit a) = Just $ PLit a
exprToPat (App f args) = PApp <$> exprToPat f <*> traverse exprToPat args
exprToPat _ = Nothing