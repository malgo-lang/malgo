{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Interface where

import Control.Exception (IOException, catch)
import Control.Monad.State (execState)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Store
import Generics.Deriving.Monoid (mappenddefault, memptydefault)
import qualified Koriel.Core.Type as C
import Koriel.Id
import Koriel.Pretty
import Language.Malgo.Desugar.DsEnv (DsEnv)
import qualified Language.Malgo.Desugar.DsEnv as DsEnv
import Language.Malgo.Prelude
import Language.Malgo.Rename.RnEnv (RnState)
import qualified Language.Malgo.Rename.RnEnv as RnState
import Language.Malgo.Syntax.Extension
import qualified Language.Malgo.Type as GT
import qualified Language.Malgo.TypeCheck.TcEnv as TcEnv
import System.FilePath ((</>))
import System.FilePath.Lens

data Interface = Interface
  { _signatureMap :: Map RnId GT.Scheme, -- from TcEnv
    _typeDefMap :: Map RnId TcEnv.TypeDef, -- from TcEnv
    _resolvedVarIdentMap :: Map PsId RnId, -- from DsEnv
    _resolvedTypeIdentMap :: Map PsId RnId, -- from TcEnv
    _coreIdentMap :: Map RnId (Id C.Type), -- from DsEnv
    _infixMap :: Map RnId (Assoc, Int) -- from RnEnv
  }
  deriving stock (Show, Generic)
  deriving anyclass (Store)

instance Semigroup Interface where
  (<>) = mappenddefault

instance Monoid Interface where
  mempty = memptydefault

makeLenses ''Interface

instance Pretty Interface where
  pPrint i =
    "Interface"
      $$ nest 2 (sep ["signatureMap =", nest 2 $ pPrint $ Map.toList (i ^. signatureMap)])
      $$ nest 2 (sep ["typeDefMap =", nest 2 $ pPrint $ Map.toList (i ^. typeDefMap)])
      $$ nest 2 (sep ["resolvedVarIdentMap =", nest 2 $ pPrint $ Map.toList (i ^. resolvedVarIdentMap)])
      $$ nest 2 (sep ["resolvedTypeIdentMap =", nest 2 $ pPrint $ Map.toList (i ^. resolvedTypeIdentMap)])
      $$ nest 2 (sep ["coreIdentMap =", nest 2 $ pPrint $ Map.toList (i ^. coreIdentMap)])

buildInterface :: RnState -> DsEnv -> Interface
buildInterface rnState dsEnv = execState ?? Interface mempty mempty mempty mempty mempty (rnState ^. RnState.infixInfo) $ do
  let modName = rnState ^. RnState.moduleName
  ifor_ (dsEnv ^. DsEnv.varEnv) $ \tcId coreId ->
    when (tcId ^. idIsExternal && tcId ^. idMeta == modName) do
      resolvedVarIdentMap . at (tcId ^. idName) ?= tcId
      coreIdentMap . at tcId ?= coreId
  ifor_ (dsEnv ^. DsEnv.tcEnv . TcEnv.varEnv) $ \rnId scheme ->
    when (rnId ^. idIsExternal && rnId ^. idMeta == modName) do
      signatureMap . at rnId ?= scheme
  ifor_ (dsEnv ^. DsEnv.tcEnv . TcEnv.typeEnv) $ \rnId typeDef -> do
    when (rnId ^. idIsExternal && rnId ^. idMeta == modName) do
      resolvedTypeIdentMap . at (rnId ^. idName) ?= rnId
      typeDefMap . at rnId ?= typeDef

storeInterface :: (MonadIO m, MonadMalgo m) => Interface -> m ()
storeInterface interface = do
  opt <- getOpt
  liftIO $
    BS.writeFile (dstName opt & extension .~ ".mlgi") $
      encode interface

loadInterface :: (HasCallStack, MonadMalgo m, MonadIO m) => ModuleName -> m Interface
loadInterface (ModuleName modName) = do
  modPaths <- modulePaths <$> getOpt
  message <- liftIO $ findAndReadFile modPaths (modName <> ".mlgi")
  case message of
    Just x -> liftIO (decodeIO x)
    Nothing ->
      errorDoc $ "Module interface file is not found:" <+> quotes (pPrint modName)
  where
    findAndReadFile :: [FilePath] -> FilePath -> IO (Maybe ByteString)
    findAndReadFile modPaths modFile = asum <$> traverse (\path -> safeReadFile (path </> modFile)) modPaths
    safeReadFile filename = (Just <$> BS.readFile filename) `catch` handler
    handler :: IOException -> IO (Maybe ByteString)
    handler _ = pure Nothing