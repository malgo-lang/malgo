module Malgo.Link (link) where

import Data.ByteString qualified as BS
import Data.HashSet qualified as HashSet
import Data.Store (Store)
import Data.Store qualified as Store
import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Static (Reader, ask)
import Koriel.Core.Syntax
import Koriel.Id
import Koriel.Pretty (errorDoc, pretty, squotes, vsep, (<+>))
import Malgo.Interface
import Malgo.Prelude
import System.Directory (doesFileExist)
import System.FilePath ((</>))

-- | Linking a program with its dependencies.
link :: (Reader ModulePathList :> es, IOE :> es, Store a) => Interface -> Program a -> Eff es (Program a)
link (interface :: Interface) mainCoreIR = do
  -- FIXME: Sort dependencies by topological order
  depCoreIRs <- traverse loadCore (HashSet.toList interface.dependencies)
  pure $ mconcat (depCoreIRs <> [mainCoreIR])

loadCore :: (Reader ModulePathList :> es, IOE :> es, Store b) => ModuleName -> Eff es b
loadCore (ModuleName modName) = do
  ModulePathList modPaths <- ask
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
          pgm <- liftIO $ Store.decodeEx <$> BS.readFile (path </> modFile)
          pure $ Right pgm
        else findAndReadFile paths modFile
