{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Malgo.Core.Syntax.Atom (HasFreeVar (..), Atom (..), HasAtom (..), Unboxed (..)) where

import Control.Lens (Traversal')
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Set qualified as Set
import Data.Store.TH
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import Malgo.Core.Type
import Malgo.Prelude
import Numeric (showHex)

-- | 'f' may have free variables
-- 'freevars' does not include callees of `call-direct`.
-- If you want to include callees of `call-direct`, merge 'callees' and 'freevars'.
class HasFreeVar f where
  -- | Free variables.
  -- It does not include callees of `call-direct`.
  freevars :: (Ord a) => f a -> Set a

  -- | Callees.
  callees :: (Ord a) => f a -> Set a

-- | atoms
data Atom a
  = -- | variable
    Var a
  | -- | literal of unboxed values
    Unboxed Unboxed
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic, Data, Typeable)
  deriving anyclass (ToJSON, FromJSON)

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

-- | Unboxed values
data Unboxed
  = Int32 Integer
  | Int64 Integer
  | Float Float
  | Double Double
  | Char Char
  | String Text
  | Bool Bool
  deriving stock (Eq, Ord, Show, Generic, Data, Typeable)
  deriving anyclass (ToJSON, FromJSON)

instance HasType Unboxed where
  typeOf Int32 {} = Int32T
  typeOf Int64 {} = Int64T
  typeOf Float {} = FloatT
  typeOf Double {} = DoubleT
  typeOf Char {} = CharT
  typeOf String {} = StringT
  typeOf Bool {} = BoolT

instance Pretty Unboxed where
  pretty (Int32 x) = pretty x <> "_i32"
  pretty (Int64 x) = pretty x <> "_i64"
  pretty (Float x) = pretty (showHex (castFloatToWord32 x) "") <> "_f32" <+> "#|" <> pretty x <> "|#"
  pretty (Double x) = pretty (showHex (castDoubleToWord64 x) "") <> "_f64" <+> "#|" <> pretty x <> "|#"
  pretty (Char x) = squotes (pretty $ convertString @_ @Text $ showLitChar x "")
  pretty (String x) = dquotes (pretty $ concatMap (`showLitChar` "") $ convertString @_ @String x)
  pretty (Bool True) = "True#"
  pretty (Bool False) = "False#"

makeStore ''Unboxed
makeStore ''Atom