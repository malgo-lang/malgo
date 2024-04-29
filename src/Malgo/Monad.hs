module Malgo.Monad (DstPath (..), Flag (..), CompileMode (..), getWorkspaceDir, runMalgoM) where

import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Static (Reader, runReader)
import Effectful.State.Static.Local
import Koriel.Core.Optimize (OptimizeOption)
import Koriel.Id
import Koriel.MonadUniq
import Malgo.Interface (Interface, ModulePathList (..))
import Malgo.Prelude
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, getCurrentDirectory, getXdgDirectory)
import System.FilePath ((</>))

newtype DstPath = DstPath FilePath

data CompileMode = LLVM deriving stock (Eq, Show)

-- | Get workspace directory.
-- If directory does not exist, create it.
getWorkspaceDir :: IO FilePath
getWorkspaceDir = do
  pwd <- getCurrentDirectory
  createDirectoryIfMissing True $ pwd </> ".malgo-work"
  createDirectoryIfMissing True $ pwd </> ".malgo-work" </> "build"
  return $ pwd </> ".malgo-work"

runMalgoM ::
  (IOE :> es) =>
  FilePath ->
  [FilePath] ->
  CompileMode ->
  Flag ->
  OptimizeOption ->
  Eff
    ( Reader OptimizeOption
        : Reader ModulePathList
        : Reader Flag
        : Reader CompileMode
        : Reader DstPath
        : State Uniq
        : State (HashMap ModuleName Interface)
        : es
    )
    b ->
  Eff es b
runMalgoM dstPath modulePaths compileMode flag opt e = do
  workspaceDir <- liftIO getWorkspaceDir
  basePath <- liftIO $ getXdgDirectory XdgData ("malgo" </> "base")
  runReader opt e
    & runReader (ModulePathList $ modulePaths <> [workspaceDir </> "build", basePath])
    & runReader flag
    & runReader compileMode
    & runReader (DstPath dstPath)
    & evalState (Uniq 0)
    & evalState @(HashMap ModuleName Interface) mempty