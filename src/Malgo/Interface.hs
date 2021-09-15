module Malgo.Interface where

import Control.Lens (At (at), Lens', ifor_, lens, view, (?=), (^.), _1)
import Data.Binary (Binary, decodeFileOrFail, encodeFile)
import Data.Binary.Get (ByteOffset)
import Data.Binary.Instances.UnorderedContainers ()
import Data.Either.Extra (mapLeft)
import Data.Graph
import Data.String.Conversions (convertString)
import qualified Koriel.Core.Type as C
import Koriel.Id
import Koriel.Pretty
import Malgo.Desugar.DsEnv (DsEnv)
import qualified Malgo.Desugar.DsEnv as DsEnv
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
    _typeAbbrMap :: HashMap (Id GT.Type) ([Id GT.Type], GT.Type), -- from TypeRef.Static
    _resolvedVarIdentMap :: HashMap PsId RnId, -- from DsEnv
    _resolvedTypeIdentMap :: HashMap PsId RnId, -- from DsEnv
    _coreIdentMap :: HashMap RnId (Id C.Type), -- from DsEnv
    _infixMap :: HashMap RnId (Assoc, Int), -- from RnState
    _dependencies :: [ModuleName] -- from RnState
  }
  deriving stock (Show, Generic)

instance Binary Interface

signatureMap :: Lens' Interface (HashMap RnId (GT.Scheme GT.Type))
signatureMap = lens _signatureMap (\i x -> i {_signatureMap = x})

typeDefMap :: Lens' Interface (HashMap RnId (GT.TypeDef GT.Type))
typeDefMap = lens _typeDefMap (\i x -> i {_typeDefMap = x})

typeAbbrMap :: Lens' Interface (HashMap (Id GT.Type) ([Id GT.Type], GT.Type))
typeAbbrMap = lens _typeAbbrMap (\i x -> i {_typeAbbrMap = x})

resolvedVarIdentMap :: Lens' Interface (HashMap PsId RnId)
resolvedVarIdentMap = lens _resolvedVarIdentMap (\i x -> i {_resolvedVarIdentMap = x})

resolvedTypeIdentMap :: Lens' Interface (HashMap PsId RnId)
resolvedTypeIdentMap = lens _resolvedTypeIdentMap (\i x -> i {_resolvedTypeIdentMap = x})

coreIdentMap :: Lens' Interface (HashMap RnId (Id C.Type))
coreIdentMap = lens _coreIdentMap (\i x -> i {_coreIdentMap = x})

infixMap :: Lens' Interface (HashMap RnId (Assoc, Int))
infixMap = lens _infixMap (\i x -> i {_infixMap = x})

dependencies :: Lens' Interface [ModuleName]
dependencies = lens _dependencies (\i x -> i {_dependencies = x})

instance Pretty Interface where
  pPrint = text . show

buildInterface :: RnState -> DsEnv -> Interface
-- TODO: write abbrMap to interface
buildInterface rnState dsEnv = execState ?? Interface mempty mempty mempty mempty mempty mempty (rnState ^. RnState.infixInfo) (rnState ^. RnState.dependencies) $ do
  let modName = rnState ^. RnState.moduleName
  ifor_ (dsEnv ^. DsEnv.nameEnv) $ \tcId coreId ->
    when (tcId ^. idSort == External modName) do
      resolvedVarIdentMap . at (tcId ^. idName) ?= tcId
      coreIdentMap . at tcId ?= coreId
  ifor_ (dsEnv ^. DsEnv.varTypeEnv) $ \tcId scheme ->
    when (tcId ^. idSort == External modName) do
      signatureMap . at tcId ?= scheme
  ifor_ (dsEnv ^. DsEnv.typeDefEnv) $ \rnId typeDef -> do
    when (rnId ^. idSort == External modName) do
      resolvedTypeIdentMap . at (rnId ^. idName) ?= rnId
      typeDefMap . at rnId ?= typeDef

storeInterface :: (MonadIO m, HasOpt env, MonadReader env m) => Interface -> m ()
storeInterface interface = do
  opt <- getOpt
  liftIO $ encodeFile (dstName opt -<.> "mlgi") interface

loadInterface :: (MonadIO m, HasOpt env, MonadReader env m) => ModuleName -> m (Maybe Interface)
loadInterface (ModuleName modName) = do
  -- logDebug $ "load interface: " <> displayShow modName
  modPaths <- modulePaths <$> getOpt
  -- logDebug $ "modPaths = " <> displayShow modPaths
  message <- findAndReadFile modPaths (convertString modName <> ".mlgi")
  case message of
    Right x -> pure $ Just x
    Left (_, _) -> do
      -- logDebug $ displayShow errorMessage
      pure Nothing
  where
    findAndReadFile :: MonadIO m => [FilePath] -> FilePath -> m (Either (ByteOffset, Doc) Interface)
    findAndReadFile [] modFile = pure $ Left (0, "module" <+> pPrint modFile <+> "is not found")
    findAndReadFile (modPath : rest) modFile = do
      isExistModFile <- liftIO $ Directory.doesFileExist (modPath </> modFile)
      if isExistModFile
        then liftIO $ mapLeft (second text) <$> decodeFileOrFail (modPath </> modFile)
        else findAndReadFile rest modFile

dependencieList :: (MonadIO m, HasOpt env, MonadReader env m) => ModuleName -> [ModuleName] -> m [ModuleName]
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
