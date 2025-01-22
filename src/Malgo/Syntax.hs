{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Syntax (Phase (..), NextOf, Definition (..), Term (..), Literal (..), Pattern (..), Clause (..), Copattern (..), Coclause (..)) where

import Control.Lens.TH (makeFieldsId)
import Data.Void (Void)
import Malgo.Lens
import Malgo.Location (HasLocation (..), Location)
import Malgo.Name (HasName (..))
import Malgo.Prelude

type data Phase = Raw | FlattenedCopatterns | FlattenedPatterns

type family NextOf (x :: Phase) :: Phase where
  NextOf Raw = FlattenedCopatterns
  NextOf FlattenedCopatterns = FlattenedPatterns

data Definition x a = Definition
  { location :: Location,
    name :: a,
    params :: [a],
    returns :: [a],
    term :: Term x a
  }

deriving instance (Show a, Show (XPatternParam x a), Show (XCopatternParam x a), Show (XPVar x a), Show (XOrigin x a)) => Show (Definition x a)

data Term x a
  = Var {location :: Location, name :: a}
  | Literal {location :: Location, literal :: Literal}
  | Construct
      { location :: Location,
        tag :: Text,
        producers :: [Term x a],
        consumers :: [Term x a]
      }
  | Comatch
      { location :: Location,
        coclauses :: [Coclause x a]
      }
  | Destruct
      { location :: Location,
        term :: Term x a,
        tag :: Text,
        producers :: [Term x a],
        consumers :: [Term x a]
      }
  | Match
      { location :: Location,
        term :: Term x a,
        clauses :: [Clause x a]
      }
  | Let
      { location :: Location,
        name :: a,
        producer :: Term x a,
        term :: Term x a
      }
  | Prim
      { location :: Location,
        tag :: Text,
        producers :: [Term x a],
        consumers :: [Term x a]
      }
  | Switch
      { location :: Location,
        term :: Term x a,
        branches :: [(Literal, Term x a)],
        defaultBranch :: Term x a
      }
  | Invoke
      { location :: Location,
        name :: a,
        producers :: [Term x a],
        consumers :: [Term x a]
      }
  | Label
      { location :: Location,
        name :: a,
        term :: Term x a
      }
  | Goto
      { location :: Location,
        term :: Term x a,
        name :: a
      }

deriving instance (Show a, Show (XPatternParam x a), Show (XCopatternParam x a), Show (XPVar x a), Show (XOrigin x a)) => Show (Term x a)

data Literal = Int {int :: Int}
  deriving (Show)

data Pattern x a where
  PConstruct ::
    { tag :: Text,
      params :: [XPatternParam x a],
      returns :: [XPatternParam x a]
    } ->
    Pattern x a
  PVar ::
    { name :: XPVar x a
    } ->
    Pattern x a

type family XPVar x a where
  XPVar Raw a = a
  XPVar FlattenedCopatterns a = a
  XPVar FlattenedPatterns a = Void -- After flattening, all variables should be introduced as parameters or let-bound variables

type family XPatternParam x a where
  XPatternParam Raw a = Pattern Raw a
  XPatternParam FlattenedCopatterns a = Pattern FlattenedCopatterns a
  XPatternParam FlattenedPatterns a = a

deriving instance (Show a, Show (XPatternParam x a), Show (XPVar x a)) => Show (Pattern x a)

data Clause x a = Clause
  { pattern :: Pattern x a,
    term :: Term x a
  }

deriving instance (Show a, Show (XPatternParam x a), Show (XCopatternParam x a), Show (XPVar x a), Show (XOrigin x a)) => Show (Clause x a)

data Copattern x a where
  CDestruct ::
    { origin :: XOrigin x a,
      tag :: Text,
      params :: [XCopatternParam x a],
      returns :: [XCopatternParam x a]
    } ->
    Copattern x a
  CVar :: {name :: a} -> Copattern Raw a

type family XOrigin x a where
  XOrigin Raw a = Copattern Raw a
  XOrigin FlattenedCopatterns a = a
  XOrigin FlattenedPatterns a = a

type family XCopatternParam x a where
  XCopatternParam Raw a = Pattern Raw a
  XCopatternParam FlattenedCopatterns a = a
  XCopatternParam FlattenedPatterns a = a

deriving instance (Show a, Show (XPatternParam x a), Show (XCopatternParam x a), Show (XOrigin x a)) => Show (Copattern x a)

data Coclause x a = Coclause
  { copattern :: Copattern x a,
    term :: Term x a
  }

deriving instance (Show a, Show (XPatternParam x a), Show (XCopatternParam x a), Show (XPVar x a), Show (XOrigin x a)) => Show (Coclause x a)

makeFieldsId ''Definition
makeFieldsId ''Term
makeFieldsId ''Pattern
makeFieldsId ''Clause
makeFieldsId ''Copattern
makeFieldsId ''Coclause