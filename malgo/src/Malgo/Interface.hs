{-# LANGUAGE TemplateHaskell #-}

module Malgo.Interface where

import Data.Binary (Binary, decodeFileOrFail, encodeFile)
import Data.Binary.Get (ByteOffset)
import Data.Binary.Instances.UnorderedContainers ()
import qualified Data.HashMap.Strict as HashMap
import qualified Koriel.Core.Type as C
import Koriel.Id
import Koriel.Pretty
import Malgo.Desugar.DsEnv (DsEnv)
import qualified Malgo.Desugar.DsEnv as DsEnv
import Malgo.Prelude
import Malgo.Rename.RnEnv (RnState)
import qualified Malgo.Rename.RnEnv as RnState
import Malgo.Syntax.Extension
import qualified Malgo.TypeRep.Static as GT
import qualified RIO.Directory as Directory
import System.FilePath ((-<.>), (</>))

data Interface = Interface
  { _signatureMap :: HashMap RnId (GT.Scheme GT.Type), -- from TypeRep.Static
    _typeDefMap :: HashMap RnId (GT.TypeDef GT.Type), -- from TypeRep.Static
    _typeAbbrMap :: HashMap (Id GT.Type) ([Id GT.Type], GT.Type), -- from TypeRef.Static
    _resolvedVarIdentMap :: HashMap PsId RnId, -- from DsEnv
    _resolvedTypeIdentMap :: HashMap PsId RnId, -- from DsEnv
    _coreIdentMap :: HashMap RnId (Id C.Type), -- from DsEnv
    _infixMap :: HashMap RnId (Assoc, Int) -- from RnEnv
  }
  deriving stock (Show, Generic)

instance Binary Interface

makeLenses ''Interface

instance Pretty Interface where
  pPrint i =
    "Interface"
      $$ nest 2 (sep ["signatureMap =", nest 2 $ pPrint $ HashMap.toList (i ^. signatureMap)])
      $$ nest 2 (sep ["typeDefMap =", nest 2 $ pPrint $ HashMap.toList (i ^. typeDefMap)])
      $$ nest 2 (sep ["resolvedVarIdentMap =", nest 2 $ pPrint $ HashMap.toList (i ^. resolvedVarIdentMap)])
      $$ nest 2 (sep ["resolvedTypeIdentMap =", nest 2 $ pPrint $ HashMap.toList (i ^. resolvedTypeIdentMap)])
      $$ nest 2 (sep ["coreIdentMap =", nest 2 $ pPrint $ HashMap.toList (i ^. coreIdentMap)])

buildInterface :: RnState -> DsEnv -> Interface
-- TODO: write abbrMap to interface
buildInterface rnState dsEnv = execState ?? Interface mempty mempty mempty mempty mempty mempty (rnState ^. RnState.infixInfo) $ do
  let modName = rnState ^. RnState.moduleName
  ifor_ (dsEnv ^. DsEnv.nameEnv) $ \tcId coreId ->
    when (tcId ^. idSort == External modName) do
      resolvedVarIdentMap . at (nameToString $ tcId ^. idName) ?= tcId
      coreIdentMap . at tcId ?= coreId
  ifor_ (dsEnv ^. DsEnv.varTypeEnv) $ \tcId scheme ->
    when (tcId ^. idSort == External modName) do
      signatureMap . at tcId ?= scheme
  ifor_ (dsEnv ^. DsEnv.typeDefEnv) $ \rnId typeDef -> do
    when (rnId ^. idSort == External modName) do
      resolvedTypeIdentMap . at (nameToString $ rnId ^. idName) ?= rnId
      typeDefMap . at rnId ?= typeDef

storeInterface :: (MonadIO m, HasOpt env, MonadReader env m) => Interface -> m ()
storeInterface interface = do
  opt <- getOpt
  liftIO $ encodeFile (dstName opt -<.> "mlgi") interface

loadInterface :: (MonadIO m, HasOpt env, MonadReader env m, HasLogFunc env) => ModuleName -> m (Maybe Interface)
loadInterface (ModuleName modName) = do
  logDebug $ "load interface: " <> displayShow modName
  modPaths <- modulePaths <$> getOpt
  logDebug $ "modPaths = " <> displayShow modPaths
  message <- findAndReadFile modPaths (modName <> ".mlgi")
  case message of
    Right x -> pure $ Just x
    Left (_, errorMessage) -> do
      logDebug $ fromString errorMessage
      pure Nothing
  where
    findAndReadFile :: MonadIO m => [FilePath] -> FilePath -> m (Either (ByteOffset, String) Interface)
    findAndReadFile [] modFile = pure $ Left (0, render $ "module" <+> pPrint modFile <+> "is not found")
    findAndReadFile (modPath : rest) modFile = do
      isExistModFile <- Directory.doesFileExist (modPath </> modFile)
      if isExistModFile
        then liftIO $ decodeFileOrFail (modPath </> modFile)
        else findAndReadFile rest modFile
