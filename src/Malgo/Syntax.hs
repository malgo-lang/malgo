{-# LANGUAGE TemplateHaskell #-}

module Malgo.Syntax (Definition (..), Term (..), Literal (..), Pattern (..), Clause (..), Copattern (..), Coclause (..)) where

import Control.Lens.TH (makeFieldsId)
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

data Term a
  = Var {location :: Location, name :: a}
  | Literal {location :: Location, literal :: Literal}
  | Construct
      { location :: Location,
        tag :: Text,
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
  | Prim
      { location :: Location,
        tag :: Text,
        producers :: [Term a],
        consumers :: [Term a]
      }
  | Switch
      { location :: Location,
        term :: Term a,
        branches :: [(Literal, Term a)],
        defaultBranch :: Term a
      }
  | Invoke
      { location :: Location,
        name :: a,
        producers :: [Term a],
        consumers :: [Term a]
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

data Literal = Int {int :: Int}
  deriving (Show)

data Pattern a = Pattern
  { tag :: Text,
    params :: [a],
    returns :: [a]
  }
  deriving (Show)

data Clause a = Clause
  { pattern :: Pattern a,
    term :: Term a
  }
  deriving (Show)

data Copattern a = Copattern
  { tag :: Text,
    params :: [a],
    returns :: [a]
  }
  deriving (Show)

data Coclause a = Coclause
  { copattern :: Copattern a,
    term :: Term a
  }
  deriving (Show)

makeFieldsId ''Definition
makeFieldsId ''Term
makeFieldsId ''Pattern
makeFieldsId ''Clause
makeFieldsId ''Copattern
makeFieldsId ''Coclause