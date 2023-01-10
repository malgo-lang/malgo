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
    HasObject (..),
    HasVariable (..),
    HasExtFuncs (..),
    HasTopFuncs (..),
    HasTopVars (..),
    HasInterfaces (..),
    HasIndexes (..),
    HasKindCtx (..),
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

class HasObject s a | s -> a where
  object :: Lens' s a

class HasVariable s a | s -> a where
  variable :: Lens' s a

class HasExtFuncs s a | s -> a where
  extFuncs :: Lens' s a

class HasTopFuncs s a | s -> a where
  topFuncs :: Lens' s a

class HasTopVars s a | s -> a where
  topVars :: Lens' s a

class HasIndexes s a | s -> a where
  indexes :: Lens' s a

class HasInterfaces s a | s -> a where
  interfaces :: Lens' s a

class HasKindCtx s a | s -> a where
  kindCtx :: Lens' s a