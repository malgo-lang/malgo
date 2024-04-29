module Malgo.Monad (DstPath (..), Flag (..), CompileMode (..), runMalgoM) where

import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Static (Reader, runReader)
import Effectful.State.Static.Local
import Koriel.Core.Optimize (OptimizeOption)
import Koriel.Id
import Koriel.MonadUniq
import Malgo.Interface (Interface, getWorkspaceDir)
import Malgo.Prelude
import System.FilePath (takeFileName, (-<.>), (</>))

newtype DstPath = DstPath FilePath

data CompileMode = LLVM deriving stock (Eq, Show)

extensionOf :: (IsString a) => CompileMode -> a
extensionOf LLVM = "ll"

runMalgoM ::
  (IOE :> es) =>
  FilePath ->
  CompileMode ->
  Flag ->
  OptimizeOption ->
  Eff
    ( Reader OptimizeOption
        : Reader FilePath
        : Reader Flag
        : Reader CompileMode
        : Reader DstPath
        : State Uniq
        : State (HashMap ModuleName Interface)
        : es
    )
    b ->
  Eff es b
runMalgoM srcPath compileMode flag opt e = do
  workspaceDir <- getWorkspaceDir
  let dstPath = workspaceDir </> takeFileName srcPath -<.> extensionOf compileMode
  runReader opt e
    & runReader workspaceDir
    & runReader flag
    & runReader compileMode
    & runReader (DstPath dstPath)
    & evalState (Uniq 0)
    & evalState @(HashMap ModuleName Interface) mempty