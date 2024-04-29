module Malgo.Link (link) where

import Data.ByteString qualified as BS
import Data.HashSet qualified as HashSet
import Data.Store (Store)
import Data.Store qualified as Store
import Effectful (Eff, IOE, (:>))
import Koriel.Core.Syntax
import Koriel.Id
import Malgo.Interface
import Malgo.Prelude
import System.FilePath ((</>))

-- | Linking a program with its dependencies.
link :: (IOE :> es, Store a) => Interface -> Program a -> Eff es (Program a)
link (interface :: Interface) mainCoreIR = do
  -- FIXME: Sort dependencies by topological order
  depCoreIRs <- traverse loadCore (HashSet.toList interface.dependencies)
  pure $ mconcat (depCoreIRs <> [mainCoreIR])

loadCore :: (IOE :> es, Store b) => ModuleName -> Eff es b
loadCore (ModuleName modName) = do
  workspaceDir <- getWorkspaceDir
  let modulePath = workspaceDir </> convertString modName <> ".kor.bin"
  liftIO $ Store.decodeEx <$> BS.readFile modulePath