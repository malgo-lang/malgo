{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Koriel.Core.Syntax.Atom (Atom (..), HasAtom (..)) where

import Control.Lens (Traversal')
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.HashSet qualified as HashSet
import Data.Store.TH
import Koriel.Core.Syntax.Common
import Koriel.Core.Syntax.Unboxed
import Koriel.Core.Type
import Malgo.Prelude

-- | atoms
data Atom a
  = -- | variable
    Var a
  | -- | literal of unboxed values
    Unboxed Unboxed
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic, Data, Typeable)
  deriving anyclass (ToJSON, FromJSON)

makeStore ''Atom

instance (HasType a) => HasType (Atom a) where
  typeOf (Var x) = typeOf x
  typeOf (Unboxed x) = typeOf x

instance (Pretty a) => Pretty (Atom a) where
  pretty (Var x) = pretty x
  pretty (Unboxed x) = pretty x

instance HasFreeVar Atom where
  freevars (Var x) = HashSet.singleton x
  freevars Unboxed {} = mempty
  callees _ = mempty

-- | 'f' may include atoms
class HasAtom f where
  atom :: Traversal' (f a) (Atom a)

instance HasAtom Atom where
  atom = identity
