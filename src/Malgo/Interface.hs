{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Interface where

import Codec.Serialise
import Control.Lens (At (at), ifor_, view, (?=), (^.), _1)
import Control.Lens.TH
import Data.Aeson
import Data.Graph
import Data.HashMap.Strict qualified as HashMap
import Data.Store (Store)
import Data.Store qualified as Store
import Data.String.Conversions (convertString)
import Koriel.Core.Type qualified as C
import Koriel.Id
import Koriel.Lens
import Koriel.Pretty
import Malgo.Desugar.DsEnv (DsEnv, HasNameEnv (nameEnv), moduleName)
import Malgo.Infer.TypeRep qualified as GT
import Malgo.Lsp.Index (Index)
import Malgo.Prelude
import Malgo.Rename.RnEnv (RnState)
import Malgo.Rename.RnEnv qualified as RnState
import Malgo.Syntax.Extension
import System.Directory qualified as Directory
import System.FilePath ((-<.>), (</>))

data Interface = Interface
  { -- | Used in Infer
    _signatureMap :: HashMap RnId (GT.Scheme GT.Type),
    -- | Used in Infer
    _typeDefMap :: HashMap RnId (GT.TypeDef GT.Type),
    -- | Used in Infer
    _typeSynonymMap :: HashMap (Id GT.Type) ([Id GT.Type], GT.Type),
    -- | Used in Rename
    _resolvedVarIdentMap :: HashMap PsId RnId,
    -- | Used in Rename
    _resolvedTypeIdentMap :: HashMap PsId RnId,
    -- | Used in Desugar
    _coreIdentMap :: HashMap RnId (Id C.Type),
    -- | Used in Rename
    infixMap :: HashMap RnId (Assoc, Int),
    -- | Used in Rename
    dependencies :: [ModuleName],
    -- | Used in Lsp
    _lspIndex :: Index
  }
  deriving stock (Show, Generic)

instance ToJSON Interface

instance FromJSON Interface

instance Serialise Interface

instance Store Interface

makeFieldsNoPrefix ''Interface

instance Pretty Interface where
  pPrint = Koriel.Pretty.text . show

buildInterface :: RnState -> DsEnv -> Index -> Interface
-- TODO: write abbrMap to interface
buildInterface rnState dsEnv index = execState ?? Interface mempty mempty mempty mempty mempty mempty (rnState ^. RnState.infixInfo) (rnState ^. RnState.dependencies) index $ do
  let modName = dsEnv.moduleName
  ifor_ (dsEnv ^. nameEnv) $ \tcId coreId ->
    when (tcId.sort == External modName) do
      resolvedVarIdentMap . at (tcId.name) ?= tcId
      coreIdentMap . at tcId ?= coreId
  ifor_ (dsEnv ^. signatureMap) $ \tcId scheme ->
    when (tcId.sort == External modName) do
      signatureMap . at tcId ?= scheme
  ifor_ (dsEnv ^. typeDefMap) $ \rnId typeDef -> do
    when (rnId.sort == External modName) do
      resolvedTypeIdentMap . at (rnId.name) ?= rnId
      typeDefMap . at rnId ?= typeDef

storeInterface :: (MonadIO m, HasDstName env FilePath, MonadReader env m) => Interface -> m ()
storeInterface interface = do
  dstName <- view dstName
  let encoded = Store.encode interface
  writeFileBS (dstName -<.> "mlgi") encoded

loadInterface ::
  ( MonadReader s m,
    MonadIO m,
    HasInterfaces s (IORef (HashMap ModuleName Interface)),
    HasModulePaths s [FilePath]
  ) =>
  ModuleName ->
  m (Maybe Interface)
loadInterface (ModuleName modName) = do
  interfacesRef <- view interfaces
  interfaces <- readIORef interfacesRef
  case HashMap.lookup (ModuleName modName) interfaces of
    Just interface -> return $ Just interface
    Nothing -> do
      modPaths <- view modulePaths
      message <- findAndReadFile modPaths (convertString modName <> ".mlgi")
      case message of
        Right x -> do
          writeIORef interfacesRef $ HashMap.insert (ModuleName modName) x interfaces
          pure $ Just x
        Left err -> do
          hPrint stderr err
          pure Nothing
  where
    findAndReadFile [] modFile = pure $ Left ("interface" <+> pPrint modFile <+> "is not found")
    findAndReadFile (modPath : rest) modFile = do
      isExistModFile <- liftIO $ Directory.doesFileExist (modPath </> modFile)
      if isExistModFile
        then do
          raw <- readFileBS (modPath </> modFile)
          Right <$> liftIO (Store.decodeIO raw)
        else findAndReadFile rest modFile

dependencieList :: (HasModulePaths s [FilePath], HasInterfaces s (IORef (HashMap ModuleName Interface)), MonadIO m, MonadReader s m) => ModuleName -> [ModuleName] -> m [ModuleName]
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
      let to = interface.dependencies
      case to of
        [] -> pure [(node, from, to)]
        _ -> do
          xs <- foldMapM genDepList to
          pure $ (node, from, to) : xs
