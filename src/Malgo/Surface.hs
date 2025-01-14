{-# LANGUAGE TemplateHaskell #-}

module Malgo.Surface (Definition (..), Type (..), Term (..), Literal (..), Pattern (..), Clause (..), Copattern (..), Coclause (..)) where

import Control.Lens.TH (makeFieldsId)
import Data.SCargot.Repr.Basic qualified as S
import Malgo.Lens
import Malgo.Location (HasLocation (..), Location)
import Malgo.Name (HasName (..))
import Malgo.Prelude

data Definition a
  = Definition
      { location :: Location,
        name :: a,
        params :: [a],
        returns :: [a],
        term :: Term a
      }
  | Data
      { location :: Location,
        name :: a,
        params :: [a],
        constructors :: [(a, [Type a])]
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
  toSExpr Data {..} =
    S.L
      [ S.A (Symbol "data"),
        toSExpr name,
        S.L (toSExpr <$> params),
        S.L (toSExpr <$> constructors)
      ]

data Type a
  = TyVar {location :: Location, name :: a}
  | TyCon {location :: Location, name :: a, types :: [Type a]}
  deriving (Show)

instance (ToSExpr a) => ToSExpr (Type a) where
  toSExpr TyVar {..} = S.L [S.A (Symbol "var"), toSExpr name]
  toSExpr TyCon {..} = S.L [S.A (Symbol "con"), toSExpr name, S.L (toSExpr <$> types)]

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

data Pattern a
  = PConstruct {tag :: Text, params :: [Pattern a], returns :: [Pattern a]}
  | PVar {name :: a}
  deriving (Show)

instance (ToSExpr a) => ToSExpr (Pattern a) where
  toSExpr PConstruct {..} = S.L [S.A (Symbol tag), S.L (toSExpr <$> params), S.L (toSExpr <$> returns)]
  toSExpr PVar {..} = toSExpr name

data Clause a = Clause
  { pattern :: Pattern a,
    term :: Term a
  }
  deriving (Show)

instance (ToSExpr a) => ToSExpr (Clause a) where
  toSExpr Clause {..} = S.L [toSExpr pattern, toSExpr term]

data Copattern a
  = CDestruct {origin :: Copattern a, tag :: Text, params :: [Pattern a], returns :: [Pattern a]}
  | CVar {name :: a}
  deriving (Show)

instance (ToSExpr a) => ToSExpr (Copattern a) where
  -- toSExpr Copattern {..} = S.L [S.A (Symbol tag), S.L (toSExpr <$> params), S.L (toSExpr <$> returns)]
  toSExpr CDestruct {..} = S.L [toSExpr origin, S.A (Symbol tag), S.L (toSExpr <$> params), S.L (toSExpr <$> returns)]
  toSExpr CVar {..} = toSExpr name

data Coclause a = Coclause
  { copattern :: Copattern a,
    term :: Term a
  }
  deriving (Show)

instance (ToSExpr a) => ToSExpr (Coclause a) where
  toSExpr Coclause {..} = S.L [toSExpr copattern, toSExpr term]

makeFieldsId ''Definition
makeFieldsId ''Type
makeFieldsId ''Term
makeFieldsId ''Pattern
makeFieldsId ''Clause
makeFieldsId ''Copattern
makeFieldsId ''Coclause