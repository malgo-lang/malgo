{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- For `undefined`
{-# OPTIONS_GHC -Wno-deprecations #-}

module Malgo.DriverSpec (spec) where

import Data.ByteString.Lazy qualified as BL
import Data.List (intercalate)
import Data.Text qualified as T
import Extra (retry, timeout)
import Malgo.Core.Optimize (OptimizeOption (..), defaultOptimizeOption)
import Malgo.Driver qualified as Driver
import Malgo.Monad
import Malgo.Prelude
import Malgo.TestUtils
import System.Directory (copyFile, createDirectoryIfMissing, getCurrentDirectory, listDirectory, makeRelativeToCurrentDirectory)
import System.FilePath (isExtensionOf, takeBaseName, (-<.>), (</>))
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
import Test.Hspec (Spec, anyException, it, parallel, runIO, sequential, shouldBe, shouldThrow)

getWorkspaceDir :: (MonadIO m) => m FilePath
getWorkspaceDir = liftIO do
  pwd <- getCurrentDirectory
  createDirectoryIfMissing True $ pwd </> ".malgo-work"
  createDirectoryIfMissing True $ pwd </> ".malgo-work" </> "libs"
  return $ pwd </> ".malgo-work"

spec :: Spec
spec = do
  -- Setup malgo base library
  runIO do
    setupRuntime
    setupBuiltin
    setupPrelude
  testcases <- runIO (filter (isExtensionOf "mlg") <$> listDirectory testcaseDir)
  parallel do
    for_ testcases \testcase -> sequential do
      goldenWithTag "driver" "txt" ("normalCase " <> takeBaseName testcase) $ testNormal (testcaseDir </> testcase)
      goldenLLVM "llvm" ("normalCase " <> takeBaseName testcase) $ readLLVM (testcaseDir </> testcase)
      goldenLLVM "llvm" ("opt normalCase " <> takeBaseName testcase) $ readLLVMOpt (testcaseDir </> testcase)
  parallel do
    for_ testcases \testcase -> sequential do
      goldenWithTag "driver" "txt" ("nonoCase " <> takeBaseName testcase) $ testNoNo (testcaseDir </> testcase)
      goldenLLVM "llvm" ("nonoCase " <> takeBaseName testcase) $ readLLVM (testcaseDir </> testcase)
      goldenLLVM "llvm" ("opt nonoCase " <> takeBaseName testcase) $ readLLVMOpt (testcaseDir </> testcase)
  parallel do
    for_ testcases \testcase -> sequential do
      goldenWithTag "driver" "txt" ("nooptCase " <> takeBaseName testcase) $ testNoOpt (testcaseDir </> testcase)
      goldenLLVM "llvm" ("nooptCase " <> takeBaseName testcase) $ readLLVM (testcaseDir </> testcase)
      goldenLLVM "llvm" ("opt nooptCase " <> takeBaseName testcase) $ readLLVMOpt (testcaseDir </> testcase)
  parallel do
    for_ testcases \testcase -> sequential do
      goldenWithTag "driver" "txt" ("noliftCase " <> takeBaseName testcase) $ testNoLift (testcaseDir </> testcase)
      goldenLLVM "llvm" ("noliftCase " <> takeBaseName testcase) $ readLLVM (testcaseDir </> testcase)
      goldenLLVM "llvm" ("opt noliftCase " <> takeBaseName testcase) $ readLLVMOpt (testcaseDir </> testcase)
  parallel do
    for_ testcases \testcase -> sequential do
      goldenWithTag "driver" "txt" ("agressiveCase " <> takeBaseName testcase) $ testAggressive (testcaseDir </> testcase)
      goldenLLVM "llvm" ("agressiveCase " <> takeBaseName testcase) $ readLLVM (testcaseDir </> testcase)
      goldenLLVM "llvm" ("opt agressiveCase " <> takeBaseName testcase) $ readLLVMOpt (testcaseDir </> testcase)
  errorcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory (testcaseDir </> "error")
  parallel $ for_ errorcases \errorcase -> do
    it ("driver errorCase " <> takeBaseName errorcase)
      $ testError (testcaseDir </> "error" </> errorcase)
      `shouldThrow` anyException

-- | Copy runtime.c to /tmp/malgo-test/libs
setupRuntime :: IO ()
setupRuntime = do
  workspaceDir <- getWorkspaceDir
  copyFile "./runtime/malgo/runtime.c" (workspaceDir </> "libs/runtime.c")

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
          testMode = True
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

testNoLift :: FilePath -> IO BL.ByteString
testNoLift testcase =
  test testcase "nolift" False False defaultOptimizeOption LLVM

testNoOpt :: FilePath -> IO BL.ByteString
testNoOpt testcase =
  test testcase "noopt" True True defaultOptimizeOption LLVM

testNoNo :: FilePath -> IO BL.ByteString
testNoNo testcase =
  test testcase "nono" False True defaultOptimizeOption LLVM

testAggressive :: FilePath -> IO BL.ByteString
testAggressive testcase =
  test testcase "aggressive" True False aggressiveOptimizeOption LLVM

aggressiveOptimizeOption :: OptimizeOption
aggressiveOptimizeOption =
  defaultOptimizeOption {inlineThreshold = 30, doSpecializeFunction = True}
