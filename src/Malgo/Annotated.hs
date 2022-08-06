{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Annotated where

import Control.Lens.TH (makeFieldsNoPrefix)
import Koriel.Lens
import Koriel.Pretty
import Language.LSP.Types.Lens (HasRange (range))
import Malgo.Prelude

data Annotated x v = Annotated {_ann :: x, _value :: v}
  deriving stock (Eq, Show, Ord)

makeFieldsNoPrefix ''Annotated

instance (Pretty x, Pretty v) => Pretty (Annotated v x) where
  pPrintPrec l _ (Annotated v x) = pPrintPrec l 0 v <> brackets (pPrintPrec l 0 x)

instance HasRange v r => HasRange (Annotated x v) r where
  range = value . range

newtype ViaAnn value ann = ViaAnn {getViaAnn :: Annotated ann value}

newtype ViaVal ann value = ViaVal {getViaVal :: Annotated ann value}

instance Functor (ViaAnn v) where
  fmap f (ViaAnn (Annotated x v)) = ViaAnn (Annotated (f x) v)

instance Foldable (ViaAnn v) where
  foldMap f (ViaAnn (Annotated x _)) = f x

instance Traversable (ViaAnn v) where
  traverse f (ViaAnn (Annotated x v)) = ViaAnn . (`Annotated` v) <$> f x

instance Functor (ViaVal v) where
  fmap f (ViaVal (Annotated x v)) = ViaVal (Annotated x (f v))

instance Foldable (ViaVal v) where
  foldMap f (ViaVal (Annotated _ v)) = f v

instance Traversable (ViaVal v) where
  traverse f (ViaVal (Annotated x v)) = ViaVal . Annotated x <$> f v
