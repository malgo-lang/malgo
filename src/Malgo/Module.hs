{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Malgo.Module
  ( ModuleName (..),
    HasModuleName,
    Workspace,
    getWorkspace,
    runWorkspaceOnPwd,
    ArtifactPath (..),
    Resource (..),
    pwdPath,
    parseArtifactPath,
    targetRelPath,
    originRelPath,
  )
where

import Data.Aeson hiding (encode)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Data
import Data.Store
import Data.Store.TH
import Effectful
import Effectful.Dispatch.Static
import GHC.Records
import Malgo.Prelude
import Path
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist, getCurrentDirectory, makeAbsolute)
import System.FilePath qualified as F

newtype ModuleName = ModuleName {raw :: Text}
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)
  deriving newtype (Hashable, Pretty, ToJSON, FromJSON)

makeStore ''ModuleName

type HasModuleName r = HasField "moduleName" r ModuleName

instance HasField "moduleName" ModuleName ModuleName where
  getField = identity

newtype WorkspaceHolder = WorkspaceHolder {getWorkspace :: FilePath}

data Workspace :: Effect

type instance DispatchOf Workspace = Static NoSideEffects

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
  evalStaticRep (Workspace $ WorkspaceHolder workspaceDir) action

getWorkspaceAbs :: (Workspace :> es) => Eff es (Path Abs Dir)
getWorkspaceAbs = do
  workspace <- getWorkspace
  parseAbsDir workspace

data ArtifactPath = ArtifactPath
  { rawPath :: FilePath,
    originPath :: Path Abs File,
    targetPath :: Path Abs File
  }

pwdPath :: (Workspace :> es) => Eff es ArtifactPath
pwdPath = do
  workspace <- getWorkspaceAbs
  let pwd = parent workspace
  let dummyOriginPath = pwd </> [relfile|dummy|]
  let dummyTargetPath = workspace </> [relfile|dummy|]
  pure
    $ ArtifactPath
      { rawPath = ".",
        originPath = dummyOriginPath,
        targetPath = dummyTargetPath
      }

originRelPath :: (Workspace :> es) => ArtifactPath -> Eff es (Path Rel File)
originRelPath ArtifactPath {originPath} = do
  workspace <- getWorkspaceAbs
  let originBasePath = parent workspace
  stripProperPrefix originBasePath originPath

targetRelPath :: (Workspace :> es) => ArtifactPath -> Eff es (Path Rel File)
targetRelPath ArtifactPath {targetPath} = do
  workspace <- getWorkspaceAbs
  let targetBasePath = workspace
  stripProperPrefix targetBasePath targetPath

parseArtifactPath :: (IOE :> es, Workspace :> es) => ArtifactPath -> FilePath -> Eff es ArtifactPath
parseArtifactPath from path = do
  let basePath = toFilePath $ parent from.originPath
  let rawPath = basePath F.</> path
  rawPath' <- liftIO $ canonicalizePath rawPath
  originPath <- parseAbsFile rawPath'

  workspace <- getWorkspaceAbs
  let originBasePath = parent workspace
  relPath <- stripProperPrefix originBasePath originPath

  let targetPath = workspace </> relPath
  pure $ ArtifactPath {rawPath = rawPath', originPath, targetPath}

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