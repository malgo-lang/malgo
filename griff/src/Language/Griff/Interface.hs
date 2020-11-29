{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Interface where

import Control.Exception (IOException, catch)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Store
import qualified Koriel.Core.Type as C
import Koriel.Id
import Language.Griff.Extension
import Language.Griff.Prelude
import System.FilePath.Lens

serializeIdentMap :: (MonadIO m, MonadGriff m) => Map (Id ModuleName) (Id C.Type) -> m ()
serializeIdentMap dcVarEnv = do
  opt <- getOpt
  liftIO $
    BS.writeFile (dstName opt & extension .~ ".grfi") $
      encode $ Map.filterWithKey (\k _ -> k ^. idIsGlobal) dcVarEnv

deserializeIdentMap :: (MonadGriff m, MonadIO m) => ModuleName -> m (Maybe (Map (Id ModuleName) (Id C.Type)))
deserializeIdentMap (ModuleName modName) = do
  opt <- getOpt
  message <- liftIO $ (Just <$> BS.readFile (srcName opt & basename .~ modName & extension .~ ".grfi")) `catch` (\e -> pure $ const Nothing (e :: IOException))
  case message of
    Nothing -> pure Nothing
    Just x -> Just <$> liftIO (decodeIO x)