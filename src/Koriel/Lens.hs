module Koriel.Lens
  ( HasAnn (..),
    HasValue (..),
    HasUniqSupply (..),
    HasOpt (..),
    HasCoreIdentMap (..),
    HasDependencies (..),
    HasInfixMap (..),
    HasResolvedTypeIdentMap (..),
    HasResolvedVarIdentMap (..),
    HasSignatureMap (..),
    HasTypeDefMap (..),
    HasTypeSynonymMap (..),
    HasFieldBelongMap (..),
    HasModuleName (..),
    HasNameEnv (..),
    HasName (..),
    HasTypeSignature (..),
    HasDefinitions (..),
  )
where

import Control.Lens
import Language.LSP.Types.Lens (HasName (..), HasValue (..))

class HasAnn s a | s -> a where
  ann :: Lens' s a

class HasUniqSupply s a | s -> a where
  uniqSupply :: Lens' s a

class HasOpt s a | s -> a where
  opt :: Lens' s a

class HasCoreIdentMap s a | s -> a where
  coreIdentMap :: Lens' s a

class HasDependencies s a | s -> a where
  dependencies :: Lens' s a

class HasInfixMap s a | s -> a where
  infixMap :: Lens' s a

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

class HasFieldBelongMap s a | s -> a where
  fieldBelongMap :: Lens' s a

class HasModuleName s a | s -> a where
  moduleName :: Lens' s a

class HasNameEnv s a | s -> a where
  nameEnv :: Lens' s a

class HasTypeSignature s a | s -> a where
  typeSignature :: Lens' s a

class HasDefinitions s a | s -> a where
  definitions :: Lens' s a