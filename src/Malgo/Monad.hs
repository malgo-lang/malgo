module Malgo.Monad (DstPath (..), Flag (..), CompileMode (..), getWorkspaceDir, runMalgoM) where

import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Static (Reader, runReader)
import Effectful.State.Static.Local
import Koriel.Core.Optimize (OptimizeOption)
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude
import Malgo.Interface (Interface, ModulePathList (..))
import Malgo.Lsp.Index (Index)
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
        : State Uniq
        : State (HashMap ModuleName Index)
        : State (HashMap ModuleName Interface)
        : es
    )
    b ->
  Eff es b
runMalgoM dstPath modulePaths compileMode flag opt e = do
  workspaceDir <- liftIO getWorkspaceDir
  basePath <- liftIO $ getXdgDirectory XdgData ("malgo" </> "base")
  let readOpt = {-# SCC "readOpt" #-} runReader opt e
  let readModulePaths = {-# SCC "readModulePaths" #-} runReader (ModulePathList $ modulePaths <> [workspaceDir </> "build", basePath]) readOpt
  let readFlag = {-# SCC "readFlag" #-} runReader flag readModulePaths
  let readCompileMode = {-# SCC "readCompileMode" #-} runReader compileMode readFlag
  let readDstPath = {-# SCC "readDstPath" #-} runReader (DstPath dstPath) readCompileMode
  let stateUniq = {-# SCC "stateUniq" #-} evalState (Uniq 0) readDstPath
  let stateIndex = {-# SCC "stateIndex" #-} evalState @(HashMap ModuleName Index) mempty stateUniq
  let stateInterface = {-# SCC "stateInterface" #-} evalState @(HashMap ModuleName Interface) mempty stateIndex
  stateInterface