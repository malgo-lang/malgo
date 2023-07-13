module Malgo.Monad (DstPath (..), Flag (..), CompileMode (..), getWorkspaceDir, runMalgoM) where

import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Static (Reader, runReader)
import Koriel.Core.Optimize (OptimizeOption)
import Koriel.Prelude
import Malgo.Interface (ModulePathList (..))
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, getCurrentDirectory, getXdgDirectory)
import System.FilePath ((</>))

newtype DstPath = DstPath FilePath

data Flag = Flag
  { noOptimize :: Bool,
    lambdaLift :: Bool,
    debugMode :: Bool,
    testMode :: Bool
  }

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
        : es
    )
    b ->
  Eff es b
runMalgoM dstPath modulePaths compileMode flag opt e = do
  workspaceDir <- liftIO getWorkspaceDir
  basePath <- liftIO $ getXdgDirectory XdgData ("malgo" </> "base")
  runReader (DstPath dstPath)
    $ runReader compileMode
    $ runReader flag
    $ runReader (ModulePathList $ modulePaths <> [workspaceDir </> "build", basePath])
    $ runReader opt e
