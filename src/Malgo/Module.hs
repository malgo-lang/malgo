{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Malgo.Module
  ( ModuleName (..),
    HasModuleName,
    Workspace,
    getWorkspace,
    registerModule,
    getModulePath,
    runWorkspaceOnPwd,
    ArtifactPath (..),
    Resource (..),
    pwdPath,
    parseArtifactPath,
    ViaStore (..),
    moduleNameToString,
  )
where

import Control.Monad.Catch
import Data.Aeson hiding (encode)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Data
import Data.HashMap.Strict qualified as HashMap
import Data.Store
import Effectful
import Effectful.Dispatch.Static
import Effectful.Error.Static (prettyCallStack)
import GHC.Records
import GHC.Stack (callStack)
import Malgo.Prelude
import Path
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist, findFile, getCurrentDirectory, makeAbsolute, withCurrentDirectory)
import System.Directory.Extra (listDirectories)
import System.FilePath (makeRelative)
import System.FilePath qualified as F

data ModuleName
  = ModuleName Text
  | Artifact ArtifactPath
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)
  deriving anyclass (Hashable, ToJSON, FromJSON, Store)

instance Pretty ModuleName where
  pretty (ModuleName raw) = pretty raw
  pretty (Artifact path) = pretty $ toFilePath path.relPath

moduleNameToString :: (ConvertibleStrings FilePath b, ConvertibleStrings Text b) => ModuleName -> b
moduleNameToString (ModuleName raw) = convertString raw
moduleNameToString (Artifact path) = convertString $ toFilePath path.relPath

type HasModuleName r = HasField "moduleName" r ModuleName

instance HasField "moduleName" ModuleName ModuleName where
  getField = identity

data WorkspaceHolder = WorkspaceHolder
  { getWorkspace :: FilePath,
    modulePathMap :: IORef (HashMap ModuleName ArtifactPath)
  }

data Workspace :: Effect

type instance DispatchOf Workspace = Static WithSideEffects

newtype instance StaticRep Workspace = Workspace WorkspaceHolder

{-# WARNING getWorkspace "This function is unsafe and should not be used in production code." #-}
getWorkspace :: (Workspace :> es) => Eff es FilePath
getWorkspace = do
  Workspace handler <- getStaticRep
  pure handler.getWorkspace

runWorkspaceOnPwd :: (IOE :> es) => Eff (Workspace : es) a -> Eff es a
runWorkspaceOnPwd action = do
  pwd <- liftIO getCurrentDirectory
  liftIO $ createDirectoryIfMissing True $ pwd F.</> ".malgo-work"
  workspaceDir <- liftIO $ makeAbsolute $ pwd F.</> ".malgo-work"
  modulePathMap <- newIORef mempty
  evalStaticRep (Workspace $ WorkspaceHolder workspaceDir modulePathMap) action

getWorkspaceAbs :: (Workspace :> es) => Eff es (Path Abs Dir)
getWorkspaceAbs = do
  workspace <- getWorkspace
  parseAbsDir workspace

registerModule :: (Workspace :> es, IOE :> es) => ModuleName -> ArtifactPath -> Eff es ()
registerModule moduleName path = do
  Workspace WorkspaceHolder {modulePathMap} <- getStaticRep
  modifyIORef modulePathMap $ HashMap.insert moduleName path

getModulePath :: (HasCallStack) => (Workspace :> es, IOE :> es) => ModuleName -> Eff es ArtifactPath
getModulePath moduleName = do
  Workspace WorkspaceHolder {modulePathMap} <- getStaticRep
  modulePathMap' <- readIORef modulePathMap
  case HashMap.lookup moduleName modulePathMap' of
    Just path -> pure path
    Nothing -> searchAndRegister moduleName

searchAndRegister :: (HasCallStack) => (Workspace :> es, IOE :> es) => ModuleName -> Eff es ArtifactPath
searchAndRegister (ModuleName moduleName) = do
  let fileName = convertString moduleName <> ".mlg"
  -- Find fileName in workspace
  workspace <- getWorkspaceAbs
  file <- search [toFilePath workspace] fileName
  let relPath = makeRelative (toFilePath workspace) file
  pwd <- pwdPath
  path <- parseArtifactPath pwd relPath
  registerModule (ModuleName moduleName) path
  pure path
  where
    search [] _ = throwM $ ModuleNotFound $ ModuleName moduleName
    search dirs fileName = do
      mfile <- liftIO $ findFile dirs fileName
      case mfile of
        Just file -> pure file
        Nothing -> do
          subDirs <- liftIO $ traverse listDirectories dirs
          search (concat subDirs) fileName
searchAndRegister (Artifact path) = pure path

data WorkspaceError where
  ModuleNotFound :: (HasCallStack) => ModuleName -> WorkspaceError

instance Show WorkspaceError where
  show = displayException

instance Exception WorkspaceError where
  displayException (ModuleNotFound moduleName) =
    "Module not found: " <> moduleNameToString moduleName <> "\n" <> prettyCallStack callStack

data ArtifactPath = ArtifactPath
  { rawPath :: FilePath,
    originPath :: Path Abs File,
    relPath :: Path Rel File,
    targetPath :: Path Abs File
  }
  deriving stock (Eq, Ord, Generic, Data, Typeable)
  deriving anyclass (Hashable, ToJSON, FromJSON, Store)

instance Show ArtifactPath where
  -- Do not show rawPath, originPath, targetPath.
  -- Because they include absolute path, which is not portable and may leak information.
  showsPrec d (ArtifactPath {relPath}) = showParen (d > 10) $ showString "ArtifactPath " . showsPrec 11 (toFilePath relPath)

deriving anyclass instance Store (Path Abs File)

deriving anyclass instance Store (Path Rel File)

instance Pretty ArtifactPath where
  pretty path = pretty $ toFilePath path.relPath

pwdPath :: (Workspace :> es) => Eff es ArtifactPath
pwdPath = do
  workspace <- getWorkspaceAbs
  let pwd = parent workspace
  let originPath = pwd </> [relfile|dummy|]
  let relPath = [relfile|dummy|]
  let targetPath = workspace </> [relfile|dummy|]
  pure
    $ ArtifactPath
      { rawPath = ".",
        originPath,
        relPath,
        targetPath
      }

parseArtifactPath :: (IOE :> es, Workspace :> es) => ArtifactPath -> FilePath -> Eff es ArtifactPath
parseArtifactPath from path = do
  let basePath = toFilePath $ parent from.originPath
  rawPath <- liftIO $ withCurrentDirectory basePath $ canonicalizePath path
  originPath <- parseAbsFile rawPath

  workspace <- getWorkspaceAbs
  let originBasePath = parent workspace
  relPath <- stripProperPrefix originBasePath originPath

  let targetPath = workspace </> relPath
  pure $ ArtifactPath {rawPath = path, originPath, relPath, targetPath}

class Resource a where
  toByteString :: a -> ByteString
  fromByteString :: ByteString -> a
  load :: (IOE :> es) => ArtifactPath -> String -> Eff es a
  load ArtifactPath {originPath, targetPath} ext = do
    targetPath <- replaceExtension ext targetPath
    exists <- liftIO $ doesFileExist $ toFilePath targetPath
    if exists
      then do
        content <- liftIO $ BS.readFile $ toFilePath targetPath
        pure $ fromByteString content
      else do
        originPath <- replaceExtension ext originPath
        content <- liftIO $ BS.readFile $ toFilePath originPath
        liftIO $ createDirectoryIfMissing True $ toFilePath $ parent targetPath
        liftIO $ BS.writeFile (toFilePath targetPath) content
        pure $ fromByteString content
  save :: (IOE :> es) => ArtifactPath -> String -> a -> Eff es ()
  save ArtifactPath {targetPath} ext content = do
    targetPath <- replaceExtension ext targetPath
    liftIO $ createDirectoryIfMissing True $ toFilePath $ parent targetPath
    liftIO $ BS.writeFile (toFilePath targetPath) $ toByteString content

newtype ViaStore a = ViaStore a
  deriving newtype (Store)

instance (Store a) => Resource (ViaStore a) where
  toByteString (ViaStore a) = encode a
  fromByteString = decodeEx

instance Resource ByteString where
  toByteString = identity
  fromByteString = identity