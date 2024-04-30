{-# LANGUAGE TemplateHaskell #-}

module Malgo.Module (ModuleName (..), HasModuleName, Workspace, getWorkspace, runWorkspaceOnPwd) where

import Data.Aeson
import Data.Data
import Data.Store ()
import Data.Store.TH
import Effectful
import Effectful.Dispatch.Static
import GHC.Records
import Malgo.Prelude
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath qualified as F

newtype ModuleName = ModuleName {raw :: Text}
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)
  deriving newtype (Hashable, Pretty, ToJSON, FromJSON)

makeStore ''ModuleName

type HasModuleName r = HasField "moduleName" r ModuleName

instance HasField "moduleName" ModuleName ModuleName where
  getField = identity

newtype WorkspaceHandler = WorkspaceHandler {getWorkspace :: IO FilePath}

data Workspace :: Effect

type instance DispatchOf Workspace = Static NoSideEffects

newtype instance StaticRep Workspace = Workspace WorkspaceHandler

{-# WARNING getWorkspace "This function is unsafe and should not be used in production code." #-}
getWorkspace :: (Workspace :> es) => Eff es FilePath
getWorkspace = do
  Workspace handler <- getStaticRep
  unsafeEff_ handler.getWorkspace

runWorkspaceOnPwd :: (IOE :> es) => Eff (Workspace : es) a -> Eff es a
runWorkspaceOnPwd action = do
  pwd <- liftIO getCurrentDirectory
  liftIO $ createDirectoryIfMissing True $ pwd F.</> ".malgo-work"
  evalStaticRep (Workspace $ WorkspaceHandler $ handler pwd) action
  where
    handler pwd = return $ pwd F.</> ".malgo-work"
