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
import Text.Parsec.Pos (SourcePos)

data Expr a
  = Var SourcePos a
  | Lit SourcePos Literal
  | App (Expr a) (NonEmpty (Expr a))
  | Codata SourcePos (NonEmpty (Clause a))
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

newtype Literal = Int Integer
  deriving stock (Eq, Show)

data Clause a = Clause {pattern :: Pat a, body :: Expr a}
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

data Pat a = PThis SourcePos | PVar SourcePos a | PLit SourcePos Literal | PApp (Pat a) (NonEmpty (Pat a))
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

exprToPat :: (IsString a, Eq a) => Expr a -> Maybe (Pat a)
exprToPat (Var p a)
  | a == "#" = Just $ PThis p
  | otherwise = Just $ PVar p a
exprToPat (Lit p a) = Just $ PLit p a
exprToPat (App f args) = PApp <$> exprToPat f <*> traverse exprToPat args
exprToPat _ = Nothing

class HasPosition a where
  position :: a -> SourcePos

instance HasPosition (Expr a) where
  position (Var p _) = p
  position (Lit p _) = p
  position (App f _) = position f
  position (Codata p _) = p

instance HasPosition (Clause a) where
  position (Clause pat _) = position pat

instance HasPosition (Pat a) where
  position (PThis p) = p
  position (PVar p _) = p
  position (PLit p _) = p
  position (PApp f _) = position f