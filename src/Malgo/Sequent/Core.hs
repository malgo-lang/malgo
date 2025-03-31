{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Sequent.Core
  ( Program (..),
    Producer (..),
    Consumer (..),
    Rank (..),
    Statement (..),
    Branch (..),
    Return,
  )
where

import Data.Map qualified as Map
import Data.SCargot.Repr.Basic qualified as S
import Data.Store
import Malgo.Module
import Malgo.Prelude
import Malgo.SExpr hiding (Char, Double, Float, String)
import Malgo.Sequent.Fun (Literal, Name, Pattern, Tag)

data Program x = Program
  {definitions :: [(Range, Name, Name, Statement x)]}

deriving stock instance (Show (Return x), Show (XJoin x), Show (XDo x)) => Show (Program x)

deriving stock instance Generic (Program x)

deriving anyclass instance (Store (Return x), Store (XJoin x), Store (XDo x)) => Store (Program x)

instance (ToSExpr (Return x)) => ToSExpr (Program x) where
  toSExpr (Program defs) = S.L $ map (\(_, name, return, body) -> toSExpr (name, return, body)) defs

deriving via (ViaStore (Program x)) instance (Store (Return x), Store (XJoin x), Store (XDo x)) => (Resource (Program x))

type data Rank = Join | Flat | Full

data Producer (x :: Rank) where
  Var :: Range -> Name -> Producer x
  Literal :: Range -> Literal -> Producer x
  Construct :: Range -> Tag -> [Producer x] -> [Return x] -> Producer x
  Lambda :: Range -> [Name] -> Statement x -> Producer x
  Object :: Range -> Map Text (Name, Statement x) -> Producer x
  Do :: XDo x -> Name -> Statement x -> Producer x

type family XDo x where
  XDo Full = Range
  XDo _ = Void

deriving stock instance (Show (Return x), Show (XJoin x), Show (XDo x)) => Show (Producer x)

deriving stock instance Generic (Producer x)

deriving anyclass instance (Store (Return x), Store (XJoin x), Store (XDo x)) => Store (Producer x)

deriving via (ViaStore (Producer x)) instance (Store (Return x), Store (XJoin x), Store (XDo x)) => (Resource (Producer x))

instance (HasRange (XDo x)) => HasRange (Producer x) where
  range (Var range _) = range
  range (Literal range _) = range
  range (Construct range _ _ _) = range
  range (Lambda range _ _) = range
  range (Object range _) = range
  range (Do x _ _) = range x

type family Return (x :: Rank) where
  Return Join = Name
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

deriving stock instance (Show (Return x), Show (XJoin x), Show (XDo x)) => Show (Consumer x)

deriving stock instance Generic (Consumer x)

deriving anyclass instance (Store (Return x), Store (XJoin x), Store (XDo x)) => Store (Consumer x)

deriving via (ViaStore (Consumer x)) instance (Store (Return x), Store (XJoin x), Store (XDo x)) => (Resource (Consumer x))

instance (ToSExpr (Return x)) => ToSExpr (Consumer x) where
  toSExpr (Label _ name) = toSExpr name
  toSExpr (Apply _ producers consumers) = S.L [S.A "apply", S.L $ map toSExpr producers, S.L $ map toSExpr consumers]
  toSExpr (Project _ field return) = S.L [S.A "project", toSExpr field, toSExpr return]
  toSExpr (Then _ name statement) = S.L [S.A "then", toSExpr name, toSExpr statement]
  toSExpr (Finish _) = S.A "finish"
  toSExpr (Select _ branches) = S.L $ S.A "select" : map toSExpr branches

type family XJoin x where
  XJoin Join = Range
  XJoin Flat = Range
  XJoin Full = Void

data Statement x where
  Cut :: Producer x -> Return x -> Statement x
  Join :: XJoin x -> Name -> Consumer x -> Statement x -> Statement x
  Primitive :: Range -> Text -> [Producer x] -> Return x -> Statement x
  Invoke :: Range -> Name -> Return x -> Statement x

deriving stock instance (Show (Return x), Show (XJoin x), Show (XDo x)) => Show (Statement x)

deriving stock instance Generic (Statement x)

deriving anyclass instance (Store (Return x), Store (XJoin x), Store (XDo x)) => Store (Statement x)

deriving via (ViaStore (Statement x)) instance (Store (Return x), Store (XJoin x), Store (XDo x)) => (Resource (Statement x))

instance (HasRange (XJoin x), HasRange (XDo x)) => HasRange (Statement x) where
  range (Cut producer _) = range producer
  range (Join x _ _ _) = range x
  range (Primitive range _ _ _) = range
  range (Invoke range _ _) = range

instance (ToSExpr (Return x)) => ToSExpr (Statement x) where
  toSExpr (Cut producer consumer) = S.L ["cut", toSExpr producer, toSExpr consumer]
  toSExpr (Join _ name consumer statement) = S.L ["join", toSExpr name, toSExpr consumer, toSExpr statement]
  toSExpr (Primitive _ name producers consumer) =
    S.L [S.A "prim", toSExpr name, S.L $ map toSExpr producers, toSExpr consumer]
  toSExpr (Invoke _ name consumer) = S.L ["invoke", toSExpr name, toSExpr consumer]

data Branch x = Branch
  { range :: Range,
    pattern :: Pattern,
    statement :: Statement x
  }

deriving stock instance (Show (Return x), Show (XJoin x), Show (XDo x)) => Show (Branch x)

deriving stock instance Generic (Branch x)

deriving anyclass instance (Store (Return x), Store (XJoin x), Store (XDo x)) => Store (Branch x)

deriving via (ViaStore (Branch x)) instance (Store (Return x), Store (XJoin x), Store (XDo x)) => (Resource (Branch x))

instance (ToSExpr (Return x)) => ToSExpr (Branch x) where
  toSExpr (Branch _ pattern statement) = S.L [toSExpr pattern, toSExpr statement]
