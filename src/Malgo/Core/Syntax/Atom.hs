{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Malgo.Core.Syntax.Atom (Atom (..), HasAtom (..)) where

import Control.Lens (Traversal')
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Set qualified as Set
import Data.Store.TH
import Malgo.Core.Syntax.Common
import Malgo.Core.Syntax.Unboxed
import Malgo.Core.Type
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
  freevars (Var x) = Set.singleton x
  freevars Unboxed {} = mempty
  callees _ = mempty

-- | 'f' may include atoms
class HasAtom f where
  atom :: Traversal' (f a) (Atom a)

instance HasAtom Atom where
  atom = identity
