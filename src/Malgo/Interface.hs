{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Interface where

import Control.Lens (At (at), ifor_, view, (?=), (^.), _1)
import Control.Lens.TH
import Data.Binary (Binary, decodeFileOrFail, encodeFile)
import Data.Binary.Get (ByteOffset)
import Data.Binary.Instances.UnorderedContainers ()
import Data.Either.Extra (mapLeft)
import Data.Graph
import Data.String.Conversions (convertString)
import qualified Koriel.Core.Type as C
import Koriel.Id
import Koriel.Lens
import Koriel.Pretty
import Malgo.Desugar.DsEnv (DsEnv)
import Malgo.Lsp.Index (Index)
import Malgo.Prelude
import Malgo.Rename.RnEnv (RnState)
import qualified Malgo.Rename.RnEnv as RnState
import Malgo.Syntax.Extension
import qualified Malgo.TypeRep as GT
import qualified System.Directory as Directory
import System.FilePath ((-<.>), (</>))

data Interface = Interface
  { _signatureMap :: HashMap RnId (GT.Scheme GT.Type), -- from TypeRep.Static
    _typeDefMap :: HashMap RnId (GT.TypeDef GT.Type), -- from TypeRep.Static
    _typeSynonymMap :: HashMap (Id GT.Type) ([Id GT.Type], GT.Type), -- from TypeRef.Static
    _resolvedVarIdentMap :: HashMap PsId RnId, -- from DsEnv
    _resolvedTypeIdentMap :: HashMap PsId RnId, -- from DsEnv
    _coreIdentMap :: HashMap RnId (Id C.Type), -- from DsEnv
    _infixMap :: HashMap RnId (Assoc, Int), -- from RnState
    _dependencies :: [ModuleName], -- from RnState
    _lspIndex :: Index -- from Lsp.Index
  }
  deriving stock (Show, Generic)

instance Binary Interface

makeFieldsNoPrefix ''Interface

instance Pretty Interface where
  pPrint = Koriel.Pretty.text . show

buildInterface :: RnState -> DsEnv -> Index -> Interface
-- TODO: write abbrMap to interface
buildInterface rnState dsEnv index = execState ?? Interface mempty mempty mempty mempty mempty mempty (rnState ^. RnState.infixInfo) (rnState ^. RnState.dependencies) index $ do
  let modName = dsEnv ^. moduleName
  ifor_ (dsEnv ^. nameEnv) $ \tcId coreId ->
    when (tcId ^. idSort == External modName) do
      resolvedVarIdentMap . at (tcId ^. idName) ?= tcId
      coreIdentMap . at tcId ?= coreId
  ifor_ (dsEnv ^. signatureMap) $ \tcId scheme ->
    when (tcId ^. idSort == External modName) do
      signatureMap . at tcId ?= scheme
  ifor_ (dsEnv ^. typeDefMap) $ \rnId typeDef -> do
    when (rnId ^. idSort == External modName) do
      resolvedTypeIdentMap . at (rnId ^. idName) ?= rnId
      typeDefMap . at rnId ?= typeDef

storeInterface :: (MonadIO m, HasDstName env FilePath, MonadReader env m) => Interface -> m ()
storeInterface interface = do
  dstName <- view dstName
  traceM $ "storeInterface: " <> show dstName
  liftIO $ encodeFile (dstName -<.> "mlgi") interface

loadInterface :: (MonadReader s m, HasModulePaths s [FilePath], MonadIO m) => ModuleName -> m (Maybe Interface)
loadInterface (ModuleName modName) = do
  modPaths <- view modulePaths
  traceShowM modPaths
  message <- findAndReadFile modPaths (convertString modName <> ".mlgi")
  case message of
    Right x -> pure $ Just x
    Left (_, err) -> do
      hPrint stderr err
      pure Nothing
  where
    findAndReadFile :: MonadIO m => [FilePath] -> FilePath -> m (Either (ByteOffset, Doc) Interface)
    findAndReadFile [] modFile = pure $ Left (0, "interface" <+> pPrint modFile <+> "is not found")
    findAndReadFile (modPath : rest) modFile = do
      isExistModFile <- liftIO $ Directory.doesFileExist (modPath </> modFile)
      if isExistModFile
        then liftIO $ mapLeft (second Koriel.Pretty.text) <$> decodeFileOrFail (modPath </> modFile)
        else findAndReadFile rest modFile

dependencieList :: (HasModulePaths s [FilePath], MonadIO m, MonadReader s m) => ModuleName -> [ModuleName] -> m [ModuleName]
dependencieList modName imports = do
  depList <- ordNub . ((modName, modName, imports) :) <$> foldMapM genDepList imports
  let (depGraph, nodeFromVertex, _) = graphFromEdges depList
  let topSorted = map (view _1 . nodeFromVertex) $ reverse $ topSort depGraph
  pure topSorted
  where
    genDepList modName = do
      let node = modName
      let from = modName
      interface <-
        loadInterface modName >>= \case
          Nothing -> error $ show $ pPrint modName <> " is not found"
          Just x -> pure x
      let to = interface ^. dependencies
      case to of
        [] -> pure [(node, from, to)]
        _ -> do
          xs <- foldMapM genDepList to
          pure $ (node, from, to) : xs
