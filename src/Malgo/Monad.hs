{-# LANGUAGE TemplateHaskell #-}

module Malgo.Monad (MalgoEnv (..), CompileMode (..), newMalgoEnv, MalgoM, runMalgoM) where

import Control.Lens.TH
import Control.Monad.Fix (MonadFix)
import Koriel.Id (ModuleName)
import Koriel.Lens
import Koriel.MonadUniq (UniqSupply (..))
import Koriel.Prelude
import Malgo.Interface (Interface)
import Malgo.Lsp.Index (Index)
import System.Directory (XdgDirectory (XdgData), getXdgDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))

data MalgoEnv = MalgoEnv
  { _uniqSupply :: UniqSupply,
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

newMalgoEnv :: FilePath -> [FilePath] -> IO MalgoEnv
newMalgoEnv dstPath modulePaths = do
  _uniqSupply <- UniqSupply <$> newIORef 0
  _interfaces <- newIORef mempty
  _indexes <- newIORef mempty
  basePath <- getXdgDirectory XdgData ("malgo" </> "base")
  let _modulePaths = modulePaths <> [takeDirectory dstPath, ".malgo-work" </> "build", basePath]
  let compileMode = case takeExtension dstPath of
        ".ll" -> LLVM
        _ -> error "unknown extension"
  let noOptimize = False
  let lambdaLift = False
  let inlineSize = 10
  let debugMode = False
  pure MalgoEnv {..}

newtype MalgoM a = MalgoM {unMalgoM :: ReaderT MalgoEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader MalgoEnv, MonadFix, MonadFail)

runMalgoM :: MalgoEnv -> MalgoM a -> IO a
runMalgoM env m = runReaderT (m.unMalgoM) env
