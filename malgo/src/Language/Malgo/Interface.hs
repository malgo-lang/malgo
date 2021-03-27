{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Interface where

import Control.Monad.State (execState)
import Data.Binary (Binary, decodeFileOrFail, encodeFile)
import Data.Binary.Get (ByteOffset)
import Data.Binary.Instances.UnorderedContainers ()
import qualified Data.HashMap.Strict as HashMap
import qualified Koriel.Core.Type as C
import Koriel.Id
import Koriel.Pretty
import Language.Malgo.Desugar.DsEnv (DsEnv)
import qualified Language.Malgo.Desugar.DsEnv as DsEnv
import Language.Malgo.Prelude
import Language.Malgo.Rename.RnEnv (RnState)
import qualified Language.Malgo.Rename.RnEnv as RnState
import Language.Malgo.Syntax.Extension
import qualified Language.Malgo.TypeRep.Static as GT
import System.FilePath ((-<.>), (</>))

data Interface = Interface
  { _signatureMap :: HashMap RnId GT.Scheme, -- from TypeRep.Static
    _typeDefMap :: HashMap RnId GT.TypeDef, -- from TypeRep.Static
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
buildInterface rnState dsEnv = execState ?? Interface mempty mempty mempty mempty mempty (rnState ^. RnState.infixInfo) $ do
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

storeInterface :: (MonadIO m, MonadMalgo m) => Interface -> m ()
storeInterface interface = do
  opt <- getOpt
  liftIO $ encodeFile (dstName opt -<.> "mlgi") interface

loadInterface :: (MonadMalgo m, MonadIO m) => ModuleName -> m Interface
loadInterface (ModuleName modName) = do
  modPaths <- modulePaths <$> getOpt
  message <- liftIO $ findAndReadFile modPaths (modName <> ".mlgi")
  case message of
    Right x -> pure x
    Left (_, errorMessage) -> error errorMessage
  where
    findAndReadFile :: [FilePath] -> FilePath -> IO (Either (ByteOffset, String) Interface)
    findAndReadFile modPaths modFile = asumMap (\path -> decodeFileOrFail (path </> modFile)) modPaths