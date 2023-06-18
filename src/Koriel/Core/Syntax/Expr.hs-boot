module Koriel.Core.Syntax.Expr (Stmt, Expr) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Data (Data)
import Data.Kind qualified
import Koriel.Core.Syntax.Atom (HasAtom)
import Koriel.Core.Syntax.Common (HasFreeVar)
import Koriel.Core.Type (HasType)
import Koriel.Prelude (Eq, Foldable, Functor, Ord, Show)
import Koriel.Pretty (Pretty)

data Stmt (a :: Data.Kind.Type)

instance Eq a => Eq (Stmt a)
instance Ord a => Ord (Stmt a)
instance Show a => Show (Stmt a)
instance Functor Stmt
instance Foldable Stmt
instance Data a => Data (Stmt a)

instance Binary a => Binary (Stmt a)
instance ToJSON a => ToJSON (Stmt a)
instance FromJSON a => FromJSON (Stmt a)

instance HasAtom Stmt
instance HasFreeVar Stmt
instance HasType a => HasType (Stmt a)

instance Pretty a => Pretty (Stmt a)

data Expr (a :: Data.Kind.Type)

instance Eq a => Eq (Expr a)
instance Ord a => Ord (Expr a)
instance Show a => Show (Expr a)
instance Functor Expr
instance Foldable Expr
instance Data a => Data (Expr a)

instance Binary a => Binary (Expr a)
instance ToJSON a => ToJSON (Expr a)
instance FromJSON a => FromJSON (Expr a)

instance HasAtom Expr
instance HasFreeVar Expr
instance HasType a => HasType (Expr a)

instance Pretty a => Pretty (Expr a)