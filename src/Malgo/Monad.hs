{-# LANGUAGE TemplateHaskell #-}

module Malgo.Monad (MalgoEnv (..), CompileMode (..), getWorkspaceDir, newMalgoEnv, MalgoM, runMalgoM) where

import Control.Lens.TH
import Control.Monad.Extra (fromMaybeM)
import Control.Monad.Fix (MonadFix)
import Koriel.Core.Optimize (OptimizeOption, defaultOptimizeOption)
import Koriel.Id (ModuleName)
import Koriel.MonadUniq (UniqSupply (..))
import Koriel.Prelude
import Malgo.Interface (Interface)
import Malgo.Lsp.Index (Index)
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, getCurrentDirectory, getXdgDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))

data MalgoEnv = MalgoEnv
  { uniqSupply :: UniqSupply,
    -- In 'Malgo.Driver.compile' function, 'moduleName' can be 'undefined'.
    moduleName :: ~ModuleName,
    interfaces :: IORef (HashMap ModuleName Interface),
    indexes :: IORef (HashMap ModuleName Index),
    dstPath :: FilePath,
    compileMode :: CompileMode,
    noOptimize :: Bool,
    lambdaLift :: Bool,
    optimizeOption :: OptimizeOption,
    debugMode :: Bool,
    testMode :: Bool,
    modulePaths :: [FilePath]
  }

data CompileMode = LLVM | DelimLLVM deriving stock (Eq, Show)

makeFieldsNoPrefix ''MalgoEnv

-- | Get workspace directory.
-- If directory does not exist, create it.
getWorkspaceDir :: IO FilePath
getWorkspaceDir = do
  pwd <- getCurrentDirectory
  createDirectoryIfMissing True $ pwd </> ".malgo-work"
  createDirectoryIfMissing True $ pwd </> ".malgo-work" </> "build"
  return $ pwd </> ".malgo-work"

newMalgoEnv ::
  FilePath ->
  [FilePath] ->
  Maybe UniqSupply ->
  ModuleName ->
  Maybe (IORef (HashMap ModuleName Interface)) ->
  Maybe (IORef (HashMap ModuleName Index)) ->
  IO MalgoEnv
newMalgoEnv srcFile modulePaths mUniqSupply moduleName mInterfaces mIndexes = do
  uniqSupply <- fromMaybeM (UniqSupply <$> newIORef 0) (pure mUniqSupply)
  interfaces <- fromMaybeM (newIORef mempty) (pure mInterfaces)
  indexes <- fromMaybeM (newIORef mempty) (pure mIndexes)
  basePath <- getXdgDirectory XdgData ("malgo" </> "base")
  workspaceDir <- getWorkspaceDir
  let dstPath = workspaceDir </> "build" </> takeBaseName srcFile <> ".ll"
  let compileMode = case takeExtension dstPath of
        ".ll" -> LLVM
        _ -> error "unknown extension"
  let noOptimize = False
  let lambdaLift = True
  let optimizeOption = defaultOptimizeOption
  let debugMode = False
  let testMode = False
  modulePaths <- pure $ modulePaths <> [workspaceDir </> "build", basePath]
  pure MalgoEnv {..}

newtype MalgoM a = MalgoM {unMalgoM :: ReaderT MalgoEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader MalgoEnv, MonadFix, MonadFail)

runMalgoM :: MalgoEnv -> MalgoM a -> IO a
runMalgoM env m = runReaderT (m.unMalgoM) env
