module Language.Malgo.Type where

import           Language.Malgo.Pretty
import           Prelude               (show)
import           Universum             hiding (Type)

data TypeScheme a = Forall [a] (Type a)
  deriving (Eq, Show)

instance Pretty a => Pretty (TypeScheme a)

newtype TyRef a = TyRef (IORef (Maybe (Type a)))
  deriving Eq

instance Show (TyRef a) where
  show _ = "<TyRef>"

instance Pretty (TyRef a) where
  pPrint _ = "<TyRef>"

data Type a = TyApp TyCon [Type a]
            | TyVar a
            | TyMeta (TyRef a)
  deriving (Eq, Show)

instance Pretty a => Pretty (Type a)

data PrimC = IntC | DoubleC | CharC | BoolC | StringC
  deriving (Eq, Show)

instance Pretty PrimC

data TyCon = TupleC Int
           | ArrowC
           | ArrayC
           | PrimC PrimC
  deriving (Eq, Show)

instance Pretty TyCon
