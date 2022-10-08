module Koriel.Lens
  ( HasUniqSupply (..),
    HasOpt (..),
    HasResolvedTypeIdentMap (..),
    HasResolvedVarIdentMap (..),
    HasSignatureMap (..),
    HasTypeDefMap (..),
    HasTypeSynonymMap (..),
    HasSrcPath (..),
    HasModulePaths (..),
    HasDebugMode (..),
  )
where

import Control.Lens

class HasUniqSupply s a | s -> a where
  uniqSupply :: Lens' s a

class HasOpt s a | s -> a where
  opt :: Lens' s a

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

class HasSrcPath s a | s -> a where
  srcPath :: Lens' s a

class HasDebugMode s a | s -> a where
  debugMode :: Lens' s a

class HasModulePaths s a | s -> a where
  modulePaths :: Lens' s a
