module Malgo.Monad (Flag (..), CompileMode (..), runMalgoM) where

import Effectful (Eff, IOE, runEff)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.State.Static.Local
import Malgo.Core.Optimize (OptimizeOption)
import Malgo.Interface (Interface)
import Malgo.Module
import Malgo.MonadUniq
import Malgo.Prelude

data CompileMode = LLVM deriving stock (Eq, Show)

runMalgoM ::
  CompileMode ->
  Flag ->
  OptimizeOption ->
  Eff
    '[ Reader OptimizeOption,
       Reader Flag,
       Reader CompileMode,
       State Uniq,
       State (Map ModuleName Interface),
       Workspace,
       IOE
     ]
    b ->
  IO b
runMalgoM compileMode flag opt e = runEff $ runWorkspaceOnPwd do
  runReader opt e
    & runReader flag
    & runReader compileMode
    & evalState (Uniq 0)
    & evalState @(Map ModuleName Interface) mempty