{-# LANGUAGE CPP #-}

import Data.String.Conversions (ConvertibleStrings (convertString))
import Malgo.Driver qualified as Driver
import Malgo.Prelude
import System.Directory (copyFile, listDirectory)
import System.Directory.Extra (createDirectoryIfMissing)
import System.FilePath (isExtensionOf, takeBaseName, (-<.>), (</>))
import System.Process.Typed
import Test.Hspec

-- | 'stack build' and 'stack test' pass '-DSTACK' to ghc.
-- This enables switching test scripts mode 'stack' or 'cabal'.
buildCommand :: String
buildCommand = "cabal"

main :: IO ()
main =
  hspec do
    -- Setup directory for test
    runIO setupTestDir
    -- Setup malgo base library
    runIO do
      setupRuntime
      setupBuiltin
      setupPrelude
    testcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory "./testcases/malgo"
    describe "Test malgo to-ll" do
      parallel $ for_ testcases \testcase -> do
        it ("test usual case " <> testcase) $ example do
          testNormal ("./testcases/malgo" </> testcase)
        --           (exitCode, out, err) <- readProcess (proc "./scripts/test/test.sh" ["./testcases/malgo" </> testcase, buildCommand])
        --           case exitCode of
        --             ExitSuccess -> pass
        --             ExitFailure _ -> expectationFailure ("stdout:\n" <> decodeUtf8 out <> "\nstderr:\n" <> decodeUtf8 err)
        --
        it ("test nono case " <> testcase <> " (no optimization, no lambda-lifting)") $ example do
          (exitCode, out, err) <- readProcess (proc "./scripts/test/test-nono.sh" ["./testcases/malgo" </> testcase, buildCommand])
          case exitCode of
            ExitSuccess -> pass
            ExitFailure _ -> expectationFailure ("stdout:\n" <> decodeUtf8 out <> "\nstderr:\n" <> decodeUtf8 err)

        it ("test noopt case " <> testcase <> " (no optimization)") $ example do
          (exitCode, out, err) <- readProcess (proc "./scripts/test/test-noopt.sh" ["./testcases/malgo" </> testcase, buildCommand])
          case exitCode of
            ExitSuccess -> pass
            ExitFailure _ -> expectationFailure ("stdout:\n" <> decodeUtf8 out <> "\nstderr:\n" <> decodeUtf8 err)

        it ("test nolift case " <> testcase <> " (no lambda-lift)") $ example do
          (exitCode, out, err) <- readProcess (proc "./scripts/test/test-nolift.sh" ["./testcases/malgo" </> testcase, buildCommand])
          case exitCode of
            ExitSuccess -> pass
            ExitFailure _ -> expectationFailure ("stdout:\n" <> decodeUtf8 out <> "\nstderr:\n" <> decodeUtf8 err)
    examples <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory "./examples/malgo"
    describe "Test example malgo to-ll" do
      parallel $ for_ examples \examplecase -> do
        it ("test " <> examplecase) $ example do
          (exitCode, out, err) <- readProcess (proc "./scripts/test/test.sh" ["./examples/malgo" </> examplecase, buildCommand])
          case exitCode of
            ExitSuccess -> pass
            ExitFailure _ -> expectationFailure ("stdout:\n" <> decodeUtf8 out <> "\nstderr:\n" <> decodeUtf8 err)
    errorcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory "./testcases/malgo/error"
    describe "Test malgo to-ll (must be error)" do
      parallel $ for_ errorcases \errorcase -> do
        it ("test error case " <> errorcase) $ example do
          (exitCode, out, err) <- readProcess (proc "./scripts/test/test-error.sh" ["./testcases/malgo/error" </> errorcase, buildCommand])
          case exitCode of
            ExitSuccess -> expectationFailure ("stdout:\n" <> decodeUtf8 out <> "\nstderr:\n" <> decodeUtf8 err)
            ExitFailure _ -> pass

testDirectory :: FilePath
testDirectory = "/tmp/malgo_test"

setupTestDir :: IO ()
setupTestDir = do
  -- create /tmp/malgo-test
  createDirectoryIfMissing True testDirectory
  createDirectoryIfMissing True (testDirectory </> "libs")

-- | Compile Builtin.mlg and copy it to /tmp/malgo-test/libs
setupBuiltin :: IO ()
setupBuiltin = do
  compile "./runtime/malgo/Builtin.mlg" (testDirectory </> "libs/Builtin.ll") [testDirectory </> "libs"] False

-- | Compile Prelude.mlg and copy it to /tmp/malgo-test/libs
setupPrelude :: IO ()
setupPrelude = do
  compile "./runtime/malgo/Prelude.mlg" (testDirectory </> "libs/Prelude.ll") [testDirectory </> "libs"] False

-- | Copy runtime.c to /tmp/malgo-test/libs
setupRuntime :: IO ()
setupRuntime = do
  copyFile "./runtime/malgo/runtime.c" (testDirectory </> "libs/runtime.c")

-- | Wrapper of 'Malgo.Driver.compile'
compile :: FilePath -> FilePath -> [FilePath] -> Bool -> IO ()
compile src dst modPaths lambdaLift = do
  malgoEnv <- newMalgoEnv dst modPaths
  Driver.compile src malgoEnv {lambdaLift}

-- | Get the correct name of `clang`
getClangCommand :: IO String
getClangCommand =
  go ["clang", "clang-12"]
  where
    go [] = error "clang not found"
    go (x : xs) = do
      (exitCode, _, _) <- readProcess (proc "which" [x])
      case exitCode of
        ExitSuccess -> pure x
        ExitFailure _ -> go xs

testNormal :: FilePath -> IO ()
testNormal testcase = do
  compile testcase (testDirectory </> takeBaseName testcase -<.> ".ll") [testDirectory </> "libs"] True
  clang <- getClangCommand
  out <- readProcessStdout_ (proc "pkg-config" ["bdw-gc", "--libs", "--cflags"])
  runProcess_
    ( proc
        clang
        [ "-Wno-override-module",
          "-lm",
          chomp $ convertString out,
          testDirectory </> "libs" </> "runtime.c",
          testDirectory </> takeBaseName testcase -<.> ".ll",
          "-o",
          testDirectory </> takeBaseName testcase -<.> ".out"
        ]
    )