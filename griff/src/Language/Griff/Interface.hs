{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Interface where

import Control.Exception (IOException, catch)
import Control.Monad.State (execState)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Store
import qualified Koriel.Core.Type as C
import Koriel.Id
import Koriel.Pretty
import Language.Griff.DesugarEnv (DesugarEnv)
import qualified Language.Griff.DesugarEnv as DsEnv
import Language.Griff.Extension
import Language.Griff.Prelude
import qualified Language.Griff.TcEnv as TcEnv
import qualified Language.Griff.Type as GT
import System.FilePath.Lens

data Interface = Interface
  { _signatureMap :: Map RnId GT.Scheme, -- from TcEnv
    _typeDefMap :: Map RnId TypeDef, -- from TcEnv
    _resolvedIdentMap :: Map PsId RnId, -- from DesugarEnv
    _coreIdentMap :: Map RnId (Id C.Type) -- from DesugarEnv
  }
  deriving stock (Show, Generic)
  deriving anyclass (Store)

signatureMap :: Lens' Interface (Map RnId GT.Scheme)
signatureMap = lens _signatureMap (\i x -> i {_signatureMap = x})

typeDefMap :: Lens' Interface (Map RnId TypeDef)
typeDefMap = lens _typeDefMap (\i x -> i {_typeDefMap = x})

resolvedIdentMap :: Lens' Interface (Map PsId RnId)
resolvedIdentMap = lens _resolvedIdentMap (\i x -> i {_resolvedIdentMap = x})

coreIdentMap :: Lens' Interface (Map RnId (Id C.Type))
coreIdentMap = lens _coreIdentMap (\i x -> i {_coreIdentMap = x})

data TypeDef = TypeDef {_constructor :: GT.Type, _qualVars :: [GT.TyVar], _union :: [(RnId, GT.Type)]}
  deriving stock (Show, Generic)
  deriving anyclass (Store)

instance Pretty TypeDef where
  pPrint (TypeDef c q u) = pPrint (c, q, u)

prettyInterface :: Interface -> Doc
prettyInterface i =
  "Interface"
    $$ nest 2 (sep ["signatureMap =", nest 2 $ pPrint $ Map.toList (i ^. signatureMap)])
    $$ nest 2 (sep ["typeDefMap =", nest 2 $ pPrint $ Map.toList (i ^. typeDefMap)])
    $$ nest 2 (sep ["resolvedIdentMap =", nest 2 $ pPrint $ Map.toList (i ^. resolvedIdentMap)])
    $$ nest 2 (sep ["coreIdentMap =", nest 2 $ pPrint $ Map.toList (i ^. coreIdentMap)])

buildInterface :: DesugarEnv -> Interface
buildInterface dsEnv = execState ?? Interface mempty mempty mempty mempty $ do
  ifor_ (dsEnv ^. DsEnv.varEnv) $ \tcId coreId -> do
    resolvedIdentMap %= Map.insert (tcId ^. idName) tcId
    coreIdentMap %= Map.insert tcId coreId
  ifor_ (dsEnv ^. DsEnv.tcEnv . TcEnv.varEnv) $ \rnId scheme ->
    signatureMap %= Map.insert rnId scheme
  ifor_ (dsEnv ^. DsEnv.tcEnv . TcEnv.typeEnv) $ \rnId typeRep ->
    modifying typeDefMap $
      Map.insert rnId $
        case typeRep of
          GT.TyCon tyCon ->
            case Map.lookup tyCon $ dsEnv ^. DsEnv.tcEnv . TcEnv.tyConEnv of
              Just (ps, u) -> TypeDef typeRep ps u
              Nothing -> TypeDef typeRep [] []
          _ -> TypeDef typeRep [] []

storeInterface :: (MonadIO m, MonadGriff m) => Interface -> m ()
storeInterface interface = do
  opt <- getOpt
  liftIO $
    BS.writeFile (dstName opt & extension .~ ".grfi") $
      encode interface

loadInterface :: (MonadGriff m, MonadIO m) => ModuleName -> m (Maybe Interface)
loadInterface (ModuleName modName) = do
  opt <- getOpt
  message <- liftIO $ safeReadFile (srcName opt & basename .~ modName & extension .~ ".grfi")
  case message of
    Just x -> Just <$> liftIO (decodeIO x)
    Nothing -> pure Nothing
  where
    safeReadFile filename = fmap Just (BS.readFile filename) `catch` handler
    handler :: IOException -> IO (Maybe ByteString)
    handler _ = pure Nothing