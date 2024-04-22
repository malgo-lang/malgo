{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- For `undefined`
{-# OPTIONS_GHC -Wno-deprecations #-}

module Malgo.DriverSpec (spec) where

import Control.Exception (IOException, catch)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List (intercalate)
import Data.String.Conversions.Monomorphic (toString)
import Data.Text qualified as T
import Effectful
import Error.Diagnose (TabSize (..), WithUnicode (..), addFile, defaultStyle, printDiagnostic)
import Error.Diagnose.Compat.Megaparsec (errorDiagnosticFromBundle)
import Extra (retry, timeout)
import Koriel.Core.Annotate qualified as Koriel
import Koriel.Core.Lint qualified as Koriel
import Koriel.Core.Optimize (OptimizeOption (..), defaultOptimizeOption)
import Koriel.Core.Parser qualified as Koriel
import Koriel.Id (ModuleName (ModuleName))
import Malgo.Driver qualified as Driver
import Malgo.Monad
import Malgo.Prelude
import System.Directory (copyFile, createDirectory, listDirectory, removeDirectoryRecursive)
import System.Directory.Extra (createDirectoryIfMissing)
import System.Exit (exitFailure)
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
import Test.Hspec
  ( Spec,
    anyException,
    it,
    parallel,
    runIO,
    sequential,
    shouldBe,
    shouldThrow,
  )
import Test.Hspec.Core.Spec (getSpecDescriptionPath)
import Test.Hspec.Golden (Golden (..), golden)

testcaseDir :: FilePath
testcaseDir = "./test/testcases/malgo"

outputDir :: FilePath
outputDir = "./test/tmp/malgo_test"

spec :: Spec
spec = parallel do
  -- Setup directory for test
  runIO setupTestDir
  -- Setup malgo base library
  runIO do
    setupRuntime
    setupBuiltin
    setupPrelude
  testcases <- runIO (filter (isExtensionOf "mlg") <$> listDirectory testcaseDir)
  for_ testcases \testcase -> do
    sequential do
      golden ("driver-normalCase-" <> takeBaseName testcase) $ testNormal (testcaseDir </> testcase)
      goldenLLVM ("llvm-normalCase-" <> takeBaseName testcase) $ readLLVMNormal (testcaseDir </> testcase)
      goldenLLVM ("llvm-opt-normalCase-" <> takeBaseName testcase) $ readLLVMOptNormal (testcaseDir </> testcase)
    sequential do
      golden ("driver-nonoCase-" <> takeBaseName testcase) $ testNoNo (testcaseDir </> testcase)
      goldenLLVM ("llvm-nonoCase-" <> takeBaseName testcase) $ readLLVMNoNo (testcaseDir </> testcase)
      goldenLLVM ("llvm-opt-nonoCase-" <> takeBaseName testcase) $ readLLVMOptNoNo (testcaseDir </> testcase)
    sequential do
      golden ("driver-nooptCase-" <> takeBaseName testcase) $ testNoOpt (testcaseDir </> testcase)
      goldenLLVM ("llvm-nooptCase-" <> takeBaseName testcase) $ readLLVMNoOpt (testcaseDir </> testcase)
      goldenLLVM ("llvm-opt-nooptCase-" <> takeBaseName testcase) $ readLLVMOptNoOpt (testcaseDir </> testcase)
    sequential do
      golden ("driver-noliftCase-" <> takeBaseName testcase) $ testNoLift (testcaseDir </> testcase)
      goldenLLVM ("llvm-noliftCase-" <> takeBaseName testcase) $ readLLVMNoLift (testcaseDir </> testcase)
      goldenLLVM ("llvm-opt-noliftCase-" <> takeBaseName testcase) $ readLLVMOptNoLift (testcaseDir </> testcase)
    sequential do
      golden ("driver-agressiveCase-" <> takeBaseName testcase) $ testAggressive (testcaseDir </> testcase)
      goldenLLVM ("llvm-agressiveCase-" <> takeBaseName testcase) $ readLLVMAggressive (testcaseDir </> testcase)
      goldenLLVM ("llvm-opt-agressiveCase-" <> takeBaseName testcase) $ readLLVMOptAggressive (testcaseDir </> testcase)
  examples <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory "./examples/malgo"
  for_ examples \examplecase -> do
    golden ("driver-exampleCase-" <> takeBaseName examplecase) $ testNormal ("./examples/malgo" </> examplecase)
  errorcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory (testcaseDir </> "error")
  for_ errorcases \errorcase -> do
    it ("driver-errorCase-" <> takeBaseName errorcase)
      $ testError (testcaseDir </> "error" </> errorcase)
      `shouldThrow` anyException

setupTestDir :: IO ()
setupTestDir = do
  -- remove /tmp/malgo-test if it exists
  removeDirectoryRecursive outputDir `catch` \(_ :: IOException) -> pure ()

  -- create /tmp/malgo-test
  createDirectory outputDir
  createDirectory (outputDir </> "libs")

-- | Compile Builtin.mlg and copy it to /tmp/malgo-test/libs
setupBuiltin :: IO ()
setupBuiltin = do
  compile "./runtime/malgo/Builtin.mlg" (outputDir </> "libs/Builtin.ll") [outputDir </> "libs"] False False defaultOptimizeOption LLVM

-- | Compile Prelude.mlg and copy it to /tmp/malgo-test/libs
setupPrelude :: IO ()
setupPrelude = do
  compile "./runtime/malgo/Prelude.mlg" (outputDir </> "libs/Prelude.ll") [outputDir </> "libs"] False False defaultOptimizeOption LLVM

-- | Copy runtime.c to /tmp/malgo-test/libs
setupRuntime :: IO ()
setupRuntime = do
  copyFile "./runtime/malgo/runtime.c" (outputDir </> "libs/runtime.c")

-- | Wrapper of 'Malgo.Driver.compile'
compile :: FilePath -> FilePath -> [FilePath] -> Bool -> Bool -> OptimizeOption -> CompileMode -> IO ()
compile src dst modPaths lambdaLift noOptimize option compileMode =
  do
    runEff
      $ do
        Driver.compile src
        -- Check if the generated Koriel code is valid
        let korielPath = dst -<.> "kor"
        koriel <- liftIO $ BS.readFile korielPath
        case Koriel.parse korielPath (convertString koriel) of
          Left err ->
            let diag = errorDiagnosticFromBundle @Text Nothing "Parse error on input" Nothing err
                diag' = addFile diag korielPath (toString koriel)
             in printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle diag' >> liftIO exitFailure
          Right ast -> do
            Koriel.lint True =<< Koriel.annotate (ModuleName $ convertString $ takeBaseName src) ast
      & runMalgoM
        dst
        modPaths
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
  IO String
test testcase typ lambdaLift noOptimize option compileMode = retry 3 do
  createDirectoryIfMissing True (outputDir </> typ)
  let llPath = outputDir </> typ </> takeBaseName testcase -<.> ".ll"
  timeoutWrapper "compile"
    $ compile testcase llPath [outputDir </> typ, outputDir </> "libs"] lambdaLift noOptimize option compileMode

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
          <> [ outputDir </> "libs" </> "runtime.c",
               llPath,
               "-o",
               outputDir </> typ </> takeBaseName testcase -<.> ".out"
             ]
      )
  BL.putStr err
  (clang, exitCode) `shouldBe` (clang, ExitSuccess)

  (exitCode, result) <-
    timeoutWrapper "run"
      $ second convertString
      <$> readProcessStdout
        ( proc (outputDir </> typ </> takeBaseName testcase -<.> ".out") []
            & setStdin (byteStringInput "Hello")
        )
  ("out" :: String, exitCode) `shouldBe` ("out", ExitSuccess)
  pure $ convertString $ T.stripEnd result
  where
    timeoutWrapper phase m = do
      timeout (60 * 5) m >>= \case
        Just x -> pure x
        Nothing -> error $ "timeout in " <> phase

goldenLLVM :: String -> IO BL.ByteString -> Spec
goldenLLVM description runAction = do
  path <- (<> words description) <$> getSpecDescriptionPath
  let name = intercalate "-" path
  it description do
    actualOutput <- runAction
    pure
      Golden
        { output = actualOutput,
          encodePretty = convertString,
          writeToFile = BL.writeFile,
          readFromFile = BL.readFile,
          goldenFile = ".golden" </> "llvm" </> name </> "golden.ll",
          actualFile = Just (".golden" </> "llvm" </> name </> "actual.ll"),
          failFirstTime = False
        }

readLLVM :: FilePath -> String -> IO BL.ByteString
readLLVM testcase typ = do
  let llPath = outputDir </> typ </> takeBaseName testcase -<.> ".ll"
  BL.readFile llPath

readLLVMOpt :: FilePath -> String -> IO BL.ByteString
readLLVMOpt testcase typ = do
  let llOptPath = outputDir </> typ </> takeBaseName testcase -<.> ".opt.ll"
  BL.readFile llOptPath

testError :: FilePath -> IO ()
testError testcase = do
  compile testcase (outputDir </> takeBaseName testcase -<.> ".ll") [outputDir </> "libs"] False False defaultOptimizeOption LLVM

testNormal :: FilePath -> IO String
testNormal testcase =
  test testcase "normal" True False defaultOptimizeOption LLVM

readLLVMNormal :: FilePath -> IO BL.ByteString
readLLVMNormal testcase = readLLVM testcase "normal"

readLLVMOptNormal :: FilePath -> IO BL.ByteString
readLLVMOptNormal testcase = readLLVMOpt testcase "normal"

testNoLift :: FilePath -> IO String
testNoLift testcase =
  test testcase "nolift" False False defaultOptimizeOption LLVM

readLLVMNoLift :: FilePath -> IO BL.ByteString
readLLVMNoLift testcase = readLLVM testcase "nolift"

readLLVMOptNoLift :: FilePath -> IO BL.ByteString
readLLVMOptNoLift testcase = readLLVMOpt testcase "nolift"

testNoOpt :: FilePath -> IO String
testNoOpt testcase =
  test testcase "noopt" True True defaultOptimizeOption LLVM

readLLVMNoOpt :: FilePath -> IO BL.ByteString
readLLVMNoOpt testcase = readLLVM testcase "noopt"

readLLVMOptNoOpt :: FilePath -> IO BL.ByteString
readLLVMOptNoOpt testcase = readLLVMOpt testcase "noopt"

testNoNo :: FilePath -> IO String
testNoNo testcase =
  test testcase "nono" False True defaultOptimizeOption LLVM

readLLVMNoNo :: FilePath -> IO BL.ByteString
readLLVMNoNo testcase = readLLVM testcase "nono"

readLLVMOptNoNo :: FilePath -> IO BL.ByteString
readLLVMOptNoNo testcase = readLLVMOpt testcase "nono"

testAggressive :: FilePath -> IO String
testAggressive testcase =
  test testcase "aggressive" True False aggressiveOptimizeOption LLVM

readLLVMAggressive :: FilePath -> IO BL.ByteString
readLLVMAggressive testcase = readLLVM testcase "aggressive"

readLLVMOptAggressive :: FilePath -> IO BL.ByteString
readLLVMOptAggressive testcase = readLLVMOpt testcase "aggressive"

aggressiveOptimizeOption :: OptimizeOption
aggressiveOptimizeOption =
  defaultOptimizeOption {inlineThreshold = 30, doSpecializeFunction = True}
