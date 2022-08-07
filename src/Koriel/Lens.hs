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
    HasToLLOpt (..),
    HasSrcName (..),
    HasDstName (..),
    HasDumpParsed (..),
    HasDumpRenamed (..),
    HasDumpTyped (..),
    HasForceRebuild (..),
    HasModulePaths (..),
    HasDebugMode (..),
    HasInlineSize (..),
    HasNoLambdaLift (..),
    HasNoOptimize (..),
    HasDumpDesugar (..),
    HasDumpRefine (..),
    HasSymbolInfo (..),
    HasTypeDefENv (..),
    HasVisibility (..),
    HasField (..),
    HasTypeAnn (..),
  )
where

import Control.Lens
import Language.LSP.Types.Lens (HasName (..), HasValue (..))

class HasAnn s a | s -> a where
  ann :: Lens' s a

class HasVisibility s a | s -> a where
  visibility :: Lens' s a

class HasTypeAnn s a | s -> a where
  typeAnn :: Lens' s a

class HasField s a | s -> a where
  field :: Lens' s a

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

class HasSrcName s a | s -> a where
  srcName :: Lens' s a

class HasDstName s a | s -> a where
  dstName :: Lens' s a

class HasDumpParsed s a | s -> a where
  dumpParsed :: Lens' s a

class HasDumpRenamed s a | s -> a where
  dumpRenamed :: Lens' s a

class HasDumpTyped s a | s -> a where
  dumpTyped :: Lens' s a

class HasDumpRefine s a | s -> a where
  dumpRefine :: Lens' s a

class HasDumpDesugar s a | s -> a where
  dumpDesugar :: Lens' s a

class HasNoOptimize s a | s -> a where
  noOptimize :: Lens' s a

class HasNoLambdaLift s a | s -> a where
  noLambdaLift :: Lens' s a

class HasInlineSize s a | s -> a where
  inlineSize :: Lens' s a

class HasDebugMode s a | s -> a where
  debugMode :: Lens' s a

class HasModulePaths s a | s -> a where
  modulePaths :: Lens' s a

class HasForceRebuild s a | s -> a where
  forceRebuild :: Lens' s a

class HasToLLOpt s a | s -> a where
  toLLOpt :: Lens' s a

class HasSymbolInfo s a | s -> a where
  symbolInfo :: Lens' s a

class HasTypeDefENv s a | s -> a where
  typeDefEnv :: Lens' s a