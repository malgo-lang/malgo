module Malgo.Link where

import Control.Lens (view, (^.))
import Data.HashSet qualified as HashSet
import Data.Store (decodeIO)
import Data.String.Conversions (convertString)
import Koriel.Core.Syntax
import Koriel.Core.Type (Type)
import Koriel.Id
import Koriel.Lens
import Koriel.Pretty (errorDoc, pPrint, quotes, ($$), (<+>))
import Malgo.Interface
import Malgo.Prelude
import System.Directory (doesFileExist)
import System.FilePath ((</>))

link :: (MonadIO m, MonadReader env m, HasInterfaces env (IORef (HashMap ModuleName Interface)), HasModulePaths env [FilePath]) => ModuleName -> m (Program (Id Type))
link mainModName = do
  interface <- loadInterface mainModName
  mainCoreIR <- loadCore mainModName
  depCoreIRs <- traverse loadCore (HashSet.toList interface.dependencies)
  pure $ mconcat (mainCoreIR : depCoreIRs)

loadCore :: (MonadReader s m, MonadIO m, HasModulePaths s [FilePath]) => ModuleName -> m (Program (Id Type))
loadCore (ModuleName modName) = do
  modPaths <- view modulePaths
  message <- findAndReadFile modPaths (convertString modName <> ".kor")
  case message of
    Right x -> pure x
    Left err -> do
      hPrint stderr err
      errorDoc $
        "Cannot find module:"
          <+> quotes (pPrint modName)
          $$ "Module paths:"
          <+> pPrint modPaths
  where
    findAndReadFile [] modFile = pure $ Left (pPrint modFile <+> "not found")
    findAndReadFile (path : paths) modFile = do
      isExistModFile <- liftIO $ doesFileExist (path </> modFile)
      if isExistModFile
        then do
          raw <- readFileBS (path </> modFile)
          Right <$> liftIO (decodeIO raw)
        else findAndReadFile paths modFile
