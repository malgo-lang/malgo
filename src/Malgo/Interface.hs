{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Interface where

import Control.Lens (At (at), ifor_, view, (?=), (^.), _1)
import Control.Lens.TH
import Data.Graph
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Store (Store)
import Data.Store qualified as Store
import Data.String.Conversions (convertString)
import Koriel.Core.Type qualified as C
import Koriel.Id
import Koriel.Lens
import Koriel.Pretty
import Malgo.Desugar.DsEnv (DsState, HasNameEnv (nameEnv))
import Malgo.Infer.TypeRep qualified as GT
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
    dependencies :: HashSet ModuleName
  }
  deriving stock (Show, Generic)

instance Store Interface

makeFieldsNoPrefix ''Interface

instance Pretty Interface where
  pPrint = Koriel.Pretty.text . show

buildInterface :: ModuleName -> RnState -> DsState -> Interface
-- TODO: write abbrMap to interface
buildInterface moduleName rnState dsState = execState ?? Interface mempty mempty mempty mempty mempty mempty (rnState ^. RnState.infixInfo) (rnState ^. RnState.dependencies) $ do
  ifor_ (dsState ^. nameEnv) $ \tcId coreId ->
    when (tcId.sort == External && tcId.moduleName == moduleName) do
      resolvedVarIdentMap . at (tcId.name) ?= tcId
      coreIdentMap . at tcId ?= coreId
  ifor_ (dsState ^. signatureMap) $ \tcId scheme ->
    when (tcId.sort == External && tcId.moduleName == moduleName) do
      signatureMap . at tcId ?= scheme
  ifor_ (dsState ^. typeDefMap) $ \rnId typeDef -> do
    when (rnId.sort == External && rnId.moduleName == moduleName) do
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
  m Interface
loadInterface (ModuleName modName) = do
  interfacesRef <- view interfaces
  interfaces <- readIORef interfacesRef
  case HashMap.lookup (ModuleName modName) interfaces of
    Just interface -> pure interface
    Nothing -> do
      modPaths <- view modulePaths
      message <- findAndReadFile modPaths (convertString modName <> ".mlgi")
      case message of
        Right x -> do
          writeIORef interfacesRef $ HashMap.insert (ModuleName modName) x interfaces
          pure x
        Left err -> do
          hPrint stderr err
          errorDoc $ "Cannot find module:" <+> quotes (pPrint modName)
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
      interface <- loadInterface modName
      let to = HashSet.toList interface.dependencies
      case to of
        [] -> pure [(node, from, to)]
        _ -> do
          xs <- foldMapM genDepList to
          pure $ (node, from, to) : xs
