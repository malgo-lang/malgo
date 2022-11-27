module Malgo.Link where

import Control.Lens (view)
import Data.Binary qualified as Binary
import Data.HashSet qualified as HashSet
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

link :: (MonadIO m, MonadReader env m, HasModulePaths env [FilePath]) => Interface -> Program (Id Type) -> m (Program (Id Type))
link interface mainCoreIR = do
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
          pgm <- liftIO $ Binary.decodeFile (path </> modFile)
          pure $ Right pgm
        else findAndReadFile paths modFile
