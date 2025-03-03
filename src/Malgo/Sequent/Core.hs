{-# LANGUAGE UndecidableInstances #-}

module Malgo.Sequent.Core
  ( Program (..),
    Producer (..),
    Consumer (..),
    Rank (..),
    Statement (..),
    Branch (..),
    -- Removed flatProgram export
  )
where

import Data.Map qualified as Map
import Data.SCargot.Repr.Basic qualified as S
import Malgo.Prelude
import Malgo.SExpr hiding (Char, Double, Float, String)
import Malgo.Sequent.Fun (Literal, Name, Pattern, Tag)

data Program x = Program
  {definitions :: [(Range, Name, Name, Statement x)]}

deriving stock instance (Show (Return x)) => Show (Program x)

instance (ToSExpr (Return x)) => ToSExpr (Program x) where
  toSExpr (Program defs) = S.L $ map (\(_, name, return, body) -> toSExpr (name, return, body)) defs

type data Rank = Flat | Full

data Producer (x :: Rank) where
  Var :: Range -> Name -> Producer x
  Literal :: Range -> Literal -> Producer x
  Construct :: Range -> Tag -> [Producer x] -> [Return x] -> Producer x
  Lambda :: Range -> [Name] -> Statement x -> Producer x
  Object :: Range -> Map Text (Name, Statement x) -> Producer x
  Do :: Range -> Name -> Statement Full -> Producer Full

deriving stock instance (Show (Return x)) => Show (Producer x)

type family Return (x :: Rank) where
  Return Flat = Consumer Flat
  Return Full = Consumer Full

instance (ToSExpr (Return x)) => ToSExpr (Producer x) where
  toSExpr (Var _ name) = toSExpr name
  toSExpr (Literal _ literal) = toSExpr literal
  toSExpr (Construct _ tag producers consumers) =
    S.L [S.A "construct", toSExpr tag, S.L $ map toSExpr producers, S.L $ map toSExpr consumers]
  toSExpr (Lambda _ names statement) = S.L [S.A "lambda", S.L $ map toSExpr names, toSExpr statement]
  toSExpr (Object _ kvs) = S.L $ map (\(k, v) -> S.L [toSExpr k, toSExpr v]) $ Map.toList kvs
  toSExpr (Do _ name statement) = S.L [S.A "do", toSExpr name, toSExpr statement]

data Consumer x where
  Label :: Range -> Name -> Consumer x
  Apply :: Range -> [Producer x] -> [Return x] -> Consumer x
  Project :: Range -> Text -> Return x -> Consumer x
  Then :: Range -> Name -> Statement x -> Consumer x
  Finish :: Range -> Consumer x
  Select :: Range -> [Branch x] -> Consumer x

deriving stock instance (Show (Return x)) => Show (Consumer x)

instance (ToSExpr (Return x)) => ToSExpr (Consumer x) where
  toSExpr (Label _ name) = toSExpr name
  toSExpr (Apply _ producers consumers) = S.L [S.A "apply", S.L $ map toSExpr producers, S.L $ map toSExpr consumers]
  toSExpr (Project _ field return) = S.L [S.A "project", toSExpr field, toSExpr return]
  toSExpr (Then _ name statement) = S.L [S.A "then", toSExpr name, toSExpr statement]
  toSExpr (Finish _) = S.A "finish"
  toSExpr (Select _ branches) = S.L $ S.A "select" : map toSExpr branches

data Statement x where
  Cut :: Producer x -> Consumer x -> Statement x
  CutDo :: Range -> Name -> Statement Flat -> Consumer Flat -> Statement Flat
  Primitive :: Range -> Text -> [Producer x] -> Return x -> Statement x
  Invoke :: Range -> Name -> Return x -> Statement x

deriving stock instance (Show (Return x)) => Show (Statement x)

instance (ToSExpr (Return x)) => ToSExpr (Statement x) where
  toSExpr (Cut producer consumer) = S.L ["cut", toSExpr producer, toSExpr consumer]
  toSExpr (CutDo _ name statement consumer) = S.L ["cut-do", toSExpr name, toSExpr statement, toSExpr consumer]
  toSExpr (Primitive _ name producers consumer) =
    S.L [S.A "prim", toSExpr name, S.L $ map toSExpr producers, toSExpr consumer]
  toSExpr (Invoke _ name consumer) = S.L ["invoke", toSExpr name, toSExpr consumer]

data Branch x = Branch
  { range :: Range,
    pattern :: Pattern,
    statement :: Statement x
  }

deriving stock instance (Show (Return x)) => Show (Branch x)

instance (ToSExpr (Return x)) => ToSExpr (Branch x) where
  toSExpr (Branch _ pattern statement) = S.L [toSExpr pattern, toSExpr statement]
