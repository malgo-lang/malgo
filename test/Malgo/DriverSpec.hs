{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- For `undefined`
{-# OPTIONS_GHC -Wno-deprecations #-}

module Malgo.DriverSpec (spec) where

import Control.Exception (catch)
import Control.Lens (view, _1, _2, _3)
import Data.ByteString.Lazy qualified as BL
import Data.List (intercalate)
import Data.Text qualified as T
import Extra (retry, timeout)
import Malgo.Core.Optimize (OptimizeOption (..), defaultOptimizeOption)
import Malgo.Driver qualified as Driver
import Malgo.Monad
import Malgo.Prelude
import Malgo.TestUtils
import System.Directory (listDirectory, makeRelativeToCurrentDirectory)
import System.FilePath (isExtensionOf, takeBaseName, (-<.>), (</>))
import System.IO.Silently (hCapture_)
import System.Process.Typed
  ( ExitCode (ExitFailure, ExitSuccess),
    byteStringInput,
    nullStream,
    proc,
    readProcessStderr,
    readProcessStdout,
    readProcessStdout_,
    runProcess,
    setStderr,
    setStdin,
    setStdout,
  )
import Test.Hspec (Spec, beforeAll, runIO, sequential, shouldBe)
import Test.Hspec.Core.Hooks (mapSubject)

spec :: Spec
spec = do
  testcases <- runIO (filter (isExtensionOf "mlg") <$> listDirectory testcaseDir)
  for_ testcases \testcase -> beforeAll
    ( do
        result <- testNormal (testcaseDir </> testcase)
        ll <- readLLVM (testcaseDir </> testcase)
        llOpt <- readLLVMOpt (testcaseDir </> testcase)
        pure (result, ll, llOpt)
    )
    do
      mapSubject (view _1) $ goldenWithTag' "driver" "txt" ("normalCase " <> takeBaseName testcase)
      mapSubject (view _2) $ goldenWithTag' "llvm" "ll" ("normalCase " <> takeBaseName testcase)
      mapSubject (view _3) $ goldenWithTag' "llvm" "ll" ("opt normalCase " <> takeBaseName testcase)
  errorcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory (testcaseDir </> "error")
  sequential $ for_ errorcases \errorcase -> do
    let errOutput =
          hCapture_
            [stdout, stderr]
            ( testError (testcaseDir </> "error" </> errorcase)
                `catch` \(_ :: ExitCode) -> pure ()
            )
    goldenWithTag "driver" "txt" ("errorCase " <> takeBaseName errorcase) (convertString <$> errOutput)

-- | Wrapper of 'Malgo.Driver.compile'
compile :: FilePath -> Bool -> Bool -> OptimizeOption -> CompileMode -> IO ()
compile src lambdaLift noOptimize option compileMode =
  do
    Driver.compile src
    & runMalgoM
      compileMode
      Flag
        { noOptimize,
          lambdaLift,
          debugMode = False,
          testMode = True,
          exitAfterDesugar = False
        }
      option

findCommand :: [String] -> IO String
findCommand list =
  go list >>= \case
    Nothing -> error $ "Command not found: " <> intercalate ", " list
    Just x -> pure x
  where
    go [] = pure Nothing
    go (x : xs) = do
      exitCode <- runProcess (proc "which" [x] & setStdout nullStream & setStderr nullStream)
      case exitCode of
        ExitSuccess -> pure $ Just x
        ExitFailure _ -> go xs

test ::
  (HasCallStack) =>
  -- | File path of the test case
  FilePath ->
  -- | Type of the test case
  String ->
  -- | Whether to perform lambda-lifting
  Bool ->
  -- | Whether to perform optimization
  Bool ->
  -- | Optimization option
  OptimizeOption ->
  -- | Compile mode
  CompileMode ->
  IO BL.ByteString
test testcase _typ lambdaLift noOptimize option compileMode = retry 3 do
  workspaceDir <- getWorkspaceDir
  let llPath = workspaceDir </> "test" </> "testcases" </> "malgo" </> takeBaseName testcase -<.> ".ll"
  llPath <- makeRelativeToCurrentDirectory llPath
  timeoutWrapper "compile"
    $ compile testcase lambdaLift noOptimize option compileMode

  -- Format and optimize the generated LLVM assembly
  -- find opt or opt-15
  opt <- findCommand ["opt-15", "opt"]
  (exitCode, err) <-
    readProcessStderr
      ( proc
          opt
          [ "-S",
            "-passes=default<O3>,attributor-cgscc",
            "-o",
            llPath -<.> "opt.ll",
            llPath
          ]
      )
  BL.putStr err
  (opt, exitCode) `shouldBe` (opt, ExitSuccess)

  pkgConfig <- words . convertString <$> readProcessStdout_ (proc "pkg-config" ["bdw-gc", "--libs", "--cflags"])
  clang <- findCommand ["clang-15", "clang"]
  (exitCode, err) <-
    readProcessStderr
      ( proc
          clang
          $ [ "-Wno-override-module",
              "-lm",
              "-flto=thin",
              "-O3",
              "-mllvm",
              "--attributor-enable=cgscc"
            ]
          <> pkgConfig
          <> [ workspaceDir </> "libs" </> "runtime.c",
               llPath,
               "-o",
               workspaceDir </> takeBaseName testcase -<.> ".out"
             ]
      )
  BL.putStr err
  (clang, exitCode) `shouldBe` (clang, ExitSuccess)

  (exitCode, result) <-
    timeoutWrapper "run"
      $ second convertString
      <$> readProcessStdout
        ( proc (workspaceDir </> takeBaseName testcase -<.> ".out") []
            & setStdin (byteStringInput "Hello")
        )
  ("out" :: String, exitCode) `shouldBe` ("out", ExitSuccess)
  pure $ convertString $ T.stripEnd result
  where
    timeoutWrapper phase m = do
      timeout (60 * 5) m >>= \case
        Just x -> pure x
        Nothing -> error $ "timeout in " <> phase

readLLVM :: FilePath -> IO BL.ByteString
readLLVM testcase = do
  workspaceDir <- getWorkspaceDir
  let llPath = workspaceDir </> "test" </> "testcases" </> "malgo" </> takeBaseName testcase -<.> ".ll"
  BL.readFile llPath

readLLVMOpt :: FilePath -> IO BL.ByteString
readLLVMOpt testcase = do
  workspaceDir <- getWorkspaceDir
  let llOptPath = workspaceDir </> "test" </> "testcases" </> "malgo" </> takeBaseName testcase -<.> ".opt.ll"
  BL.readFile llOptPath

testError :: FilePath -> IO ()
testError testcase = do
  compile testcase False False defaultOptimizeOption LLVM

testNormal :: FilePath -> IO BL.ByteString
testNormal testcase =
  test testcase "normal" True False defaultOptimizeOption LLVM