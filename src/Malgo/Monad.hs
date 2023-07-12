module Malgo.Monad (DstPath (..), Flag (..), CompileMode (..), getWorkspaceDir, runMalgoM) where

import Effectful ((:>))
import Effectful.Reader.Static (Reader, runReader)
import Effectful.State.Static.Shared (State)
import Koriel.Id (ModuleName)
import Koriel.Prelude
import Malgo.Interface (Interface, ModulePathList (..))
import Malgo.Lsp.Index (Index)
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, getCurrentDirectory, getXdgDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))

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

type MalgoE es =
  ( Reader ModuleName :> es,
    Reader DstPath :> es,
    Reader CompileMode :> es,
    Reader Flag :> es,
    Reader ModulePathList :> es,
    State (HashMap ModuleName Interface) :> es,
    State (HashMap ModuleName Index) :> es
  )

runMalgoM srcFile dstPath modulePaths e = do
  workspaceDir <- liftIO getWorkspaceDir
  basePath <- liftIO $ getXdgDirectory XdgData ("malgo" </> "base")
  runReader (DstPath $ workspaceDir </> "build" </> takeBaseName srcFile <> ".ll")
    $ runReader
      ( case takeExtension dstPath of
          ".ll" -> LLVM
          _ -> error "unknown extension"
      )
    $ runReader
      Flag
        { noOptimize = False,
          lambdaLift = True,
          debugMode = False,
          testMode = False
        }
    $ runReader (ModulePathList $ modulePaths <> [workspaceDir </> "build", basePath]) e
