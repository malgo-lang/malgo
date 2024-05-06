module Malgo.Core.Syntax.Expr (Expr) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Kind qualified
import Data.Store (Store)
import Malgo.Core.Syntax.Atom (HasAtom)
import Malgo.Core.Syntax.Common (HasFreeVar)
import Malgo.Core.Type (HasType)
import Malgo.Prelude (Eq, Foldable, Functor, Ord, Pretty, Show)

data Expr (a :: Data.Kind.Type)

instance (Eq a) => Eq (Expr a)

instance (Ord a) => Ord (Expr a)

instance (Show a) => Show (Expr a)

instance Functor Expr

instance Foldable Expr

instance (Data a) => Data (Expr a)

instance (Store a) => Store (Expr a)

instance (ToJSON a) => ToJSON (Expr a)

instance (FromJSON a) => FromJSON (Expr a)

instance HasAtom Expr

instance HasFreeVar Expr

instance (HasType a) => HasType (Expr a)

instance (Pretty a) => Pretty (Expr a)