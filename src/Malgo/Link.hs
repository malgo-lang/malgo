module Malgo.Link (link) where

import Data.HashSet qualified as HashSet
import Data.Store (Store)
import Effectful (Eff, IOE, (:>))
import Malgo.Core.Syntax
import Malgo.Interface
import Malgo.Module
import Malgo.Prelude

-- | Linking a program with its dependencies.
link :: (IOE :> es, Store a, Workspace :> es) => Interface -> Program a -> Eff es (Program a)
link (interface :: Interface) mainCoreIR = do
  -- FIXME: Sort dependencies by topological order
  depCoreIRs <- traverse loadCore (HashSet.toList interface.dependencies)
  pure $ mconcat (depCoreIRs <> [mainCoreIR])

loadCore :: (IOE :> es, Store b, Workspace :> es) => ModuleName -> Eff es b
loadCore modName = do
  modulePath <- getModulePath modName
  ViaStore x <- load modulePath ".mo"
  pure x