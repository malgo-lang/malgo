module Malgo.Monad (Flag (..), runMalgoM) where

import Effectful (Eff, IOE, runEff)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.State.Static.Local
import Malgo.Interface (Interface)
import Malgo.Module
import Malgo.Prelude

runMalgoM ::
  Flag ->
  Eff
    '[ Reader Flag,
       State Uniq,
       State (Map ModuleName Interface),
       State Pragma,
       Workspace,
       IOE
     ]
    b ->
  IO b
runMalgoM flag e = runEff $ runWorkspaceOnPwd do
  runReader flag e
    & evalState (Uniq 0)
    & evalState @(Map ModuleName Interface) mempty
    & evalState @Pragma mempty