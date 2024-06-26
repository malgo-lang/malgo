{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Malgo.Core.Syntax.Case
  ( Case (..),
    _Unpack,
    _OpenRecord,
    _Exact,
    _Bind,
  )
where

import Control.Lens (makePrisms, sans, traverseOf)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Map.Strict qualified as Map
import Data.Store.TH
import Malgo.Core.Syntax.Atom (HasAtom (..))
import Malgo.Core.Syntax.Common
import {-# SOURCE #-} Malgo.Core.Syntax.Expr
import Malgo.Core.Syntax.Unboxed
import Malgo.Core.Type
import Malgo.Prelude

-- | alternatives
data Case a
  = -- | constructor pattern
    Unpack Con [a] (Expr a)
  | -- | record pattern
    OpenRecord (Map Text a) (Expr a)
  | -- | unboxed value pattern
    Exact Unboxed (Expr a)
  | -- | variable pattern
    Bind a Type (Expr a)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic, Data, Typeable)
  deriving anyclass (ToJSON, FromJSON)

makeStore ''Case

instance (HasType a) => HasType (Case a) where
  typeOf (Unpack _ _ e) = typeOf e
  typeOf (OpenRecord _ e) = typeOf e
  typeOf (Exact _ e) = typeOf e
  typeOf (Bind _ _ e) = typeOf e

instance (Pretty a) => Pretty (Case a) where
  pretty (Unpack c xs e) =
    parens $ sep ["unpack" <+> parens (pretty c <+> sep (map pretty xs)), pretty e]
  pretty (OpenRecord pat e) =
    parens $ sep ["open", parens $ sep $ map (\(k, v) -> pretty k <+> pretty v) $ Map.toList pat, pretty e]
  pretty (Exact u e) = parens $ sep ["exact" <+> pretty u, pretty e]
  pretty (Bind x t e) = parens $ sep ["bind", pretty x, pretty t, pretty e]

instance HasFreeVar Case where
  freevars (Unpack _ xs e) = foldr sans (freevars e) xs
  freevars (OpenRecord pat e) = foldr sans (freevars e) (Map.elems pat)
  freevars (Exact _ e) = freevars e
  freevars (Bind x _ e) = sans x $ freevars e
  callees (Unpack _ xs e) = foldr sans (callees e) xs
  callees (OpenRecord pat e) = foldr sans (callees e) (Map.elems pat)
  callees (Exact _ e) = callees e
  callees (Bind x _ e) = sans x $ callees e

instance HasAtom Case where
  atom f = \case
    Unpack con xs e -> Unpack con xs <$> traverseOf atom f e
    OpenRecord pat e -> OpenRecord pat <$> traverseOf atom f e
    Exact u e -> Exact u <$> traverseOf atom f e
    Bind a t e -> Bind a t <$> traverseOf atom f e

makePrisms ''Case