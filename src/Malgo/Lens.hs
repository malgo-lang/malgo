module Malgo.Lens
  ( HasResolvedTypeIdentMap (..),
    HasResolvedVarIdentMap (..),
    HasSignatureMap (..),
    HasTypeDefMap (..),
    HasTypeSynonymMap (..),
    HasKindCtx (..),
  )
where

import Control.Lens

class HasResolvedTypeIdentMap s a | s -> a where
  resolvedTypeIdentMap :: Lens' s a

class HasResolvedVarIdentMap s a | s -> a where
  resolvedVarIdentMap :: Lens' s a

class HasSignatureMap s a | s -> a where
  signatureMap :: Lens' s a

class HasTypeDefMap s a | s -> a where
  typeDefMap :: Lens' s a

class HasTypeSynonymMap s a | s -> a where
  typeSynonymMap :: Lens' s a

class HasKindCtx s a | s -> a where
  kindCtx :: Lens' s a
