{-# LANGUAGE TemplateHaskell #-}

module Malgo.Surface (Definition (..), Term (..), Literal (..), Pattern (..), Clause (..), Copattern (..), Coclause (..)) where

import Control.Lens.TH (makeFieldsId)
import Data.SCargot.Repr.Basic qualified as S
import Malgo.Lens
import Malgo.Location (HasLocation (..), Location)
import Malgo.Name (HasName (..))
import Malgo.Prelude

data Definition a = Definition
  { location :: Location,
    name :: a,
    params :: [a],
    returns :: [a],
    term :: Term a
  }
  deriving (Show)

instance (ToSExpr a) => ToSExpr (Definition a) where
  toSExpr Definition {..} =
    S.L
      [ S.A (Symbol "def"),
        toSExpr name,
        S.L (toSExpr <$> params),
        S.L (toSExpr <$> returns),
        toSExpr term
      ]

data Term a
  = Var {location :: Location, name :: a}
  | Literal {location :: Location, literal :: Literal}
  | Apply
      { location :: Location,
        term :: Term a,
        producers :: [Term a],
        consumers :: [Term a]
      }
  | Comatch
      { location :: Location,
        coclauses :: [Coclause a]
      }
  | Destruct
      { location :: Location,
        term :: Term a,
        tag :: Text,
        producers :: [Term a],
        consumers :: [Term a]
      }
  | Match
      { location :: Location,
        term :: Term a,
        clauses :: [Clause a]
      }
  | Switch
      { location :: Location,
        term :: Term a,
        branches :: [(Literal, Term a)],
        defaultBranch :: Term a
      }
  | Label
      { location :: Location,
        name :: a,
        term :: Term a
      }
  | Goto
      { location :: Location,
        term :: Term a,
        name :: a
      }
  deriving (Show)

instance (ToSExpr a) => ToSExpr (Term a) where
  toSExpr Var {..} = S.L [S.A (Symbol "var"), toSExpr name]
  toSExpr Literal {..} = S.L [S.A (Symbol "literal"), toSExpr literal]
  toSExpr Apply {..} = S.L [S.A (Symbol "apply"), toSExpr term, S.L (toSExpr <$> producers), S.L (toSExpr <$> consumers)]
  toSExpr Comatch {..} = S.L [S.A (Symbol "comatch"), S.L (toSExpr <$> coclauses)]
  toSExpr Destruct {..} = S.L [S.A (Symbol "destruct"), toSExpr term, S.A (Symbol tag), S.L (toSExpr <$> producers), S.L (toSExpr <$> consumers)]
  toSExpr Match {..} = S.L [S.A (Symbol "match"), toSExpr term, S.L (toSExpr <$> clauses)]
  toSExpr Switch {..} = S.L [S.A (Symbol "switch"), toSExpr term, S.L (toSExpr <$> branches), toSExpr defaultBranch]
  toSExpr Label {..} = S.L [S.A (Symbol "label"), toSExpr name, toSExpr term]
  toSExpr Goto {..} = S.L [S.A (Symbol "goto"), toSExpr term, toSExpr name]

data Literal = Int {int :: Int}
  deriving (Show)

instance ToSExpr Literal where
  toSExpr Int {..} = S.L [S.A (Symbol "int"), S.A (Number int)]

data Pattern a = Pattern
  { tag :: Text,
    params :: [a],
    returns :: [a]
  }
  deriving (Show)

instance (ToSExpr a) => ToSExpr (Pattern a) where
  toSExpr Pattern {..} = S.L [S.A (Symbol tag), S.L (toSExpr <$> params), S.L (toSExpr <$> returns)]

data Clause a = Clause
  { pattern :: Pattern a,
    term :: Term a
  }
  deriving (Show)

instance (ToSExpr a) => ToSExpr (Clause a) where
  toSExpr Clause {..} = S.L [toSExpr pattern, toSExpr term]

data Copattern a = Copattern
  { tag :: Text,
    params :: [a],
    returns :: [a]
  }
  deriving (Show)

instance (ToSExpr a) => ToSExpr (Copattern a) where
  toSExpr Copattern {..} = S.L [S.A (Symbol tag), S.L (toSExpr <$> params), S.L (toSExpr <$> returns)]

data Coclause a = Coclause
  { copattern :: Copattern a,
    term :: Term a
  }
  deriving (Show)

instance (ToSExpr a) => ToSExpr (Coclause a) where
  toSExpr Coclause {..} = S.L [toSExpr copattern, toSExpr term]

makeFieldsId ''Definition
makeFieldsId ''Term
makeFieldsId ''Pattern
makeFieldsId ''Clause
makeFieldsId ''Copattern
makeFieldsId ''Coclause