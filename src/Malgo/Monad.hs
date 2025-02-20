module Malgo.Monad (Flag (..), runMalgoM) where

import Effectful (Eff, IOE, runEff)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.State.Static.Local
import Malgo.Core.Optimize (OptimizeOption)
import Malgo.Interface (Interface)
import Malgo.Module
import Malgo.MonadUniq
import Malgo.Prelude

runMalgoM ::
  Flag ->
  OptimizeOption ->
  Eff
    '[ Reader OptimizeOption,
       Reader Flag,
       State Uniq,
       State (Map ModuleName Interface),
       Workspace,
       IOE
     ]
    b ->
  IO b
runMalgoM flag opt e = runEff $ runWorkspaceOnPwd do
  runReader opt e
    & runReader flag
    & evalState (Uniq 0)
    & evalState @(Map ModuleName Interface) mempty