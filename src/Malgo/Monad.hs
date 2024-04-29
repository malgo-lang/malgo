module Malgo.Monad (DstPath (..), Flag (..), CompileMode (..), runMalgoM) where

import Effectful (Eff, IOE, runEff)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.State.Static.Local
import Malgo.Core.Optimize (OptimizeOption)
import Malgo.Id
import Malgo.Interface (Interface, getWorkspaceDir)
import Malgo.MonadUniq
import Malgo.Prelude
import System.FilePath (takeFileName, (-<.>), (</>))

newtype DstPath = DstPath FilePath

data CompileMode = LLVM deriving stock (Eq, Show)

extensionOf :: (IsString a) => CompileMode -> a
extensionOf LLVM = "ll"

runMalgoM ::
  FilePath ->
  CompileMode ->
  Flag ->
  OptimizeOption ->
  Eff
    '[ Reader OptimizeOption,
       Reader Flag,
       Reader CompileMode,
       Reader DstPath,
       State Uniq,
       State (HashMap ModuleName Interface),
       IOE
     ]
    b ->
  IO b
runMalgoM srcPath compileMode flag opt e = runEff do
  workspaceDir <- getWorkspaceDir
  let dstPath = workspaceDir </> takeFileName srcPath -<.> extensionOf compileMode
  runReader opt e
    & runReader flag
    & runReader compileMode
    & runReader (DstPath dstPath)
    & evalState (Uniq 0)
    & evalState @(HashMap ModuleName Interface) mempty