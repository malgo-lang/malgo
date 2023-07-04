{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Koriel.Core.Syntax.Case
  ( Case (..),
    _Unpack,
    _OpenRecord,
    _Exact,
    _Bind,
  )
where

import Control.Lens (makePrisms, sans, traverseOf)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Data (Data)
import Data.HashMap.Strict qualified as HashMap
import Koriel.Core.Syntax.Atom (HasAtom (..))
import Koriel.Core.Syntax.Common
import {-# SOURCE #-} Koriel.Core.Syntax.Expr
import Koriel.Core.Syntax.Unboxed
import Koriel.Core.Type
import Koriel.Prelude
import Koriel.Pretty

-- | alternatives
data Case a
  = -- | constructor pattern
    Unpack Con [a] (Expr a)
  | -- | record pattern
    OpenRecord (HashMap Text a) (Expr a)
  | -- | unboxed value pattern
    Exact Unboxed (Expr a)
  | -- | variable pattern
    Bind a Type (Expr a)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic, Data, Typeable)
  deriving anyclass (Binary, ToJSON, FromJSON)

instance (HasType a) => HasType (Case a) where
  typeOf (Unpack _ _ e) = typeOf e
  typeOf (OpenRecord _ e) = typeOf e
  typeOf (Exact _ e) = typeOf e
  typeOf (Bind _ _ e) = typeOf e

instance (Pretty a) => Pretty (Case a) where
  pPrint (Unpack c xs e) =
    parens $ sep ["unpack" <+> parens (pPrint c <+> sep (map pPrint xs)), pPrint e]
  pPrint (OpenRecord pat e) =
    parens $ sep ["open", parens $ sep $ map (\(k, v) -> pPrint k <+> pPrint v) $ HashMap.toList pat, pPrint e]
  pPrint (Exact u e) = parens $ sep ["exact" <+> pPrint u, pPrint e]
  pPrint (Bind x t e) = parens $ sep ["bind", pPrint x, pPrint t, pPrint e]

instance HasFreeVar Case where
  freevars (Unpack _ xs e) = foldr sans (freevars e) xs
  freevars (OpenRecord pat e) = foldr sans (freevars e) (HashMap.elems pat)
  freevars (Exact _ e) = freevars e
  freevars (Bind x _ e) = sans x $ freevars e
  callees (Unpack _ xs e) = foldr sans (callees e) xs
  callees (OpenRecord pat e) = foldr sans (callees e) (HashMap.elems pat)
  callees (Exact _ e) = callees e
  callees (Bind x _ e) = sans x $ callees e

instance HasAtom Case where
  atom f = \case
    Unpack con xs e -> Unpack con xs <$> traverseOf atom f e
    OpenRecord pat e -> OpenRecord pat <$> traverseOf atom f e
    Exact u e -> Exact u <$> traverseOf atom f e
    Bind a t e -> Bind a t <$> traverseOf atom f e

makePrisms ''Case