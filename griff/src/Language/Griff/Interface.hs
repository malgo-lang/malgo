{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Interface where

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
import Language.Griff.DsEnv (DsEnv)
import qualified Language.Griff.DsEnv as DsEnv
import Language.Griff.Extension
import Language.Griff.Prelude
import Language.Griff.RnEnv (RnState)
import qualified Language.Griff.RnEnv as RnState
import qualified Language.Griff.TcEnv as TcEnv
import qualified Language.Griff.Type as GT
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

signatureMap :: Lens' Interface (Map RnId GT.Scheme)
signatureMap = lens _signatureMap (\i x -> i {_signatureMap = x})

typeDefMap :: Lens' Interface (Map RnId TcEnv.TypeDef)
typeDefMap = lens _typeDefMap (\i x -> i {_typeDefMap = x})

resolvedVarIdentMap :: Lens' Interface (Map PsId RnId)
resolvedVarIdentMap = lens _resolvedVarIdentMap (\i x -> i {_resolvedVarIdentMap = x})

resolvedTypeIdentMap :: Lens' Interface (Map PsId RnId)
resolvedTypeIdentMap = lens _resolvedTypeIdentMap (\i x -> i {_resolvedTypeIdentMap = x})

coreIdentMap :: Lens' Interface (Map RnId (Id C.Type))
coreIdentMap = lens _coreIdentMap (\i x -> i {_coreIdentMap = x})

infixMap :: Lens' Interface (Map RnId (Assoc, Int))
infixMap = lens _infixMap (\i x -> i {_infixMap = x})

prettyInterface :: Interface -> Doc
prettyInterface i =
  "Interface"
    $$ nest 2 (sep ["signatureMap =", nest 2 $ pPrint $ Map.toList (i ^. signatureMap)])
    $$ nest 2 (sep ["typeDefMap =", nest 2 $ pPrint $ Map.toList (i ^. typeDefMap)])
    $$ nest 2 (sep ["resolvedVarIdentMap =", nest 2 $ pPrint $ Map.toList (i ^. resolvedVarIdentMap)])
    $$ nest 2 (sep ["resolvedTypeIdentMap =", nest 2 $ pPrint $ Map.toList (i ^. resolvedTypeIdentMap)])
    $$ nest 2 (sep ["coreIdentMap =", nest 2 $ pPrint $ Map.toList (i ^. coreIdentMap)])

buildInterface :: RnState -> DsEnv -> Interface
buildInterface rnState dsEnv = execState ?? Interface mempty mempty mempty mempty mempty (rnState ^. RnState.infixInfo) $ do
  ifor_ (dsEnv ^. DsEnv.varEnv) $ \tcId coreId -> do
    resolvedVarIdentMap . at (tcId ^. idName) ?= tcId
    coreIdentMap . at tcId ?= coreId
  ifor_ (dsEnv ^. DsEnv.tcEnv . TcEnv.varEnv) $ \rnId scheme ->
    signatureMap . at rnId ?= scheme
  ifor_ (dsEnv ^. DsEnv.tcEnv . TcEnv.typeEnv) $ \rnId typeDef -> do
    resolvedTypeIdentMap . at (rnId ^. idName) ?= rnId
    typeDefMap . at rnId ?= typeDef

storeInterface :: (MonadIO m, MonadGriff m) => Interface -> m ()
storeInterface interface = do
  opt <- getOpt
  liftIO $
    BS.writeFile (dstName opt & extension .~ ".grfi") $
      encode interface

loadInterface :: (MonadGriff m, MonadIO m) => ModuleName -> m Interface
loadInterface (ModuleName modName) = do
  opt <- getOpt
  message <- liftIO $ safeReadFile (dstName opt & basename .~ modName & extension .~ ".grfi")
  case message of
    Just x -> liftIO (decodeIO x)
    Nothing ->
      errorDoc $ "Module interface file is not found:" <+> quotes (pPrint modName)
  where
    safeReadFile filename = fmap Just (BS.readFile filename) `catch` handler
    handler :: IOException -> IO (Maybe ByteString)
    handler _ = pure Nothing