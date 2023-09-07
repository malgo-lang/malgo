module Malgo.Syntax
  ( Expr (..),
    Literal (..),
    Clause (..),
    Pat (..),
    exprToPat,
    HasPosition (..),
    arityOf,
  )
where

import Data.String (IsString)
import Malgo.Prelude
import Prettyprinter (Pretty (pretty), braces, sep)
import Text.Parsec.Pos (SourcePos)

data Expr a
  = Var SourcePos a
  | Lit SourcePos Literal
  | App (Expr a) [Expr a]
  | Codata SourcePos [Clause a]
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance (Pretty a) => Pretty (Expr a) where
  pretty (Var _ a) = pretty a
  pretty (Lit _ a) = pretty a
  pretty (App f args) = sep $ pretty f : map pretty args
  pretty (Codata _ clauses) = braces $ sep (map pretty clauses)

newtype Literal = Int Integer
  deriving stock (Eq, Show)

instance Pretty Literal where
  pretty (Int n) = pretty n

data Clause a = Clause {pattern :: Pat a, body :: Expr a}
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance (Pretty a) => Pretty (Clause a) where
  pretty (Clause pat body) = sep [pretty pat, "->", pretty body]

data Pat a = PThis SourcePos | PVar SourcePos a | PLit SourcePos Literal | PApp (Pat a) [Pat a]
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance (Pretty a) => Pretty (Pat a) where
  pretty (PThis _) = "#"
  pretty (PVar _ a) = pretty a
  pretty (PLit _ a) = pretty a
  pretty (PApp f args) = sep $ pretty f : map pretty args

arityOf :: Pat a -> Int
arityOf (PApp (PThis _) args) = length args
arityOf (PThis _) = 0
arityOf (PVar _ _) = 0
arityOf (PLit _ _) = 0
arityOf (PApp f as) = maximum $ arityOf f : map arityOf as

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