module Malgo.Link (link) where

import Data.HashSet qualified as HashSet
import Data.Store (Store)
import Effectful (Eff, IOE, (:>))
import Malgo.Core.Syntax
import Malgo.Interface
import Malgo.Module
import Malgo.Prelude
import Path (toFilePath)
import System.FilePath (takeBaseName)
import Witherable (hashNubOn)

-- | Linking a program with its dependencies.
link :: (IOE :> es, Store a, Workspace :> es) => Interface -> Program a -> Eff es (Program a)
link (interface :: Interface) mainCoreIR = do
  -- Ignore duplicates in dependencies.
  -- This is necessary because the same module can be imported multiple times and the module name is not unique (ModuleName or Artifact).
  let deps = hashNubOn viewBaseName (HashSet.toList interface.dependencies)
  depCoreIRs <- traverse loadCore deps
  pure $ mconcat (depCoreIRs <> [mainCoreIR])
  where
    viewBaseName (ModuleName x) = x
    viewBaseName (Artifact path) = convertString $ takeBaseName $ toFilePath path.relPath

loadCore :: (IOE :> es, Store b, Workspace :> es) => ModuleName -> Eff es b
loadCore modName = do
  modulePath <- getModulePath modName
  ViaStore x <- load modulePath ".mo"
  pure x