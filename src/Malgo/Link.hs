module Malgo.Link (link) where

import Data.Binary (Binary)
import Data.Binary qualified as Binary
import Data.HashSet qualified as HashSet
import GHC.Records (HasField)
import Koriel.Core.Syntax
import Koriel.Id
import Koriel.Pretty (errorDoc, pretty, squotes, vsep, (<+>))
import Malgo.Interface
import Malgo.Prelude
import System.Directory (doesFileExist)
import System.FilePath ((</>))

-- | Linking a program with its dependencies.
link :: (MonadIO m, MonadReader env m, HasField "modulePaths" env [FilePath], Binary a) => Interface -> Program a -> m (Program a)
link interface mainCoreIR = do
  -- FIXME: Sort dependencies by topological order
  depCoreIRs <- traverse loadCore (HashSet.toList interface.dependencies)
  pure $ mconcat (depCoreIRs <> [mainCoreIR])

loadCore :: (MonadReader s m, MonadIO m, HasField "modulePaths" s [FilePath], Binary a) => ModuleName -> m (Program a)
loadCore (ModuleName modName) = do
  modPaths <- asks (.modulePaths)
  message <- findAndReadFile modPaths (convertString modName <> ".kor.bin")
  case message of
    Right x -> pure x
    Left err -> do
      hPrint stderr err
      errorDoc
        $ vsep
          [ "Cannot find module:"
              <+> squotes (pretty modName),
            "Module paths:"
              <+> pretty modPaths
          ]
  where
    findAndReadFile [] modFile = pure $ Left (pretty modFile <+> "not found")
    findAndReadFile (path : paths) modFile = do
      isExistModFile <- liftIO $ doesFileExist (path </> modFile)
      if isExistModFile
        then do
          pgm <- liftIO $ Binary.decodeFile (path </> modFile)
          pure $ Right pgm
        else findAndReadFile paths modFile
