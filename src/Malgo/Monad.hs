{-# LANGUAGE TemplateHaskell #-}

module Malgo.Monad (MalgoEnv (..), CompileMode (..), defaultInlineSize, getWorkspaceDir, newMalgoEnv, MalgoM, runMalgoM) where

import Control.Lens.TH
import Control.Monad.Extra (fromMaybeM)
import Control.Monad.Fix (MonadFix)
import Koriel.Id (ModuleName)
import Koriel.Lens
import Koriel.MonadUniq (UniqSupply (..))
import Koriel.Prelude
import Malgo.Interface (Interface)
import Malgo.Lsp.Index (Index)
import System.Directory (XdgDirectory (XdgData), getCurrentDirectory, getXdgDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))

data MalgoEnv = MalgoEnv
  { uniqSupply :: UniqSupply,
    -- In 'Malgo.Driver.compile' function, 'moduleName' can be 'undefined'.
    moduleName :: ~ModuleName,
    _interfaces :: IORef (HashMap ModuleName Interface),
    _indexes :: IORef (HashMap ModuleName Index),
    dstPath :: FilePath,
    compileMode :: CompileMode,
    noOptimize :: Bool,
    lambdaLift :: Bool,
    inlineSize :: Int,
    debugMode :: Bool,
    _modulePaths :: [FilePath]
  }

data CompileMode = LLVM deriving stock (Eq, Show)

makeFieldsNoPrefix ''MalgoEnv

defaultInlineSize :: Int
defaultInlineSize = 10

getWorkspaceDir :: IO FilePath
getWorkspaceDir = do
  pwd <- getCurrentDirectory
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
  _interfaces <- fromMaybeM (newIORef mempty) (pure mInterfaces)
  _indexes <- fromMaybeM (newIORef mempty) (pure mIndexes)
  basePath <- getXdgDirectory XdgData ("malgo" </> "base")
  workspaceDir <- getWorkspaceDir
  let dstPath = workspaceDir </> "build" </> takeBaseName srcFile <> ".ll"
  let compileMode = case takeExtension dstPath of
        ".ll" -> LLVM
        _ -> error "unknown extension"
  let noOptimize = False
  let lambdaLift = True
  let inlineSize = defaultInlineSize
  let debugMode = False
  let _modulePaths = modulePaths <> [workspaceDir </> "build", basePath]
  pure MalgoEnv {..}

newtype MalgoM a = MalgoM {unMalgoM :: ReaderT MalgoEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader MalgoEnv, MonadFix, MonadFail)

runMalgoM :: MalgoEnv -> MalgoM a -> IO a
runMalgoM env m = runReaderT (m.unMalgoM) env
