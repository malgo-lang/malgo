{-# LANGUAGE CPP #-}

import Data.String.Conversions (ConvertibleStrings (convertString))
import Malgo.Driver qualified as Driver
import Malgo.Prelude
import System.Directory (copyFile, listDirectory)
import System.Directory.Extra (createDirectoryIfMissing)
import System.FilePath (isExtensionOf, takeBaseName, (-<.>), (</>))
import System.Process.Typed
  ( ExitCode (ExitFailure, ExitSuccess),
    proc,
    readProcessStdout_,
    runProcess,
    runProcess_,
  )
import Test.Hspec
  ( anyException,
    describe,
    example,
    hspec,
    it,
    parallel,
    runIO,
    shouldThrow,
  )

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
        it ("test nono case " <> testcase <> " (no optimization, no lambda-lifting)") $ example do
          testNoNo ("./testcases/malgo" </> testcase)
        it ("test noopt case " <> testcase <> " (no optimization)") $ example do
          testNoOpt ("./testcases/malgo" </> testcase)
        it ("test nolift case " <> testcase <> " (no lambda-lift)") $ example do
          testNoLift ("./testcases/malgo" </> testcase)
    examples <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory "./examples/malgo"
    describe "Test example malgo to-ll" do
      parallel $ for_ examples \examplecase -> do
        it ("test " <> examplecase) $ example do
          testNormal ("./examples/malgo" </> examplecase)
    errorcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory "./testcases/malgo/error"
    describe "Test malgo to-ll (must be error)" do
      parallel $ for_ errorcases \errorcase -> do
        it ("test error case " <> errorcase) $
          testError ("./testcases/malgo/error" </> errorcase) `shouldThrow` anyException

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
  compile "./runtime/malgo/Builtin.mlg" (testDirectory </> "libs/Builtin.ll") [testDirectory </> "libs"] False False

-- | Compile Prelude.mlg and copy it to /tmp/malgo-test/libs
setupPrelude :: IO ()
setupPrelude = do
  compile "./runtime/malgo/Prelude.mlg" (testDirectory </> "libs/Prelude.ll") [testDirectory </> "libs"] False False

-- | Copy runtime.c to /tmp/malgo-test/libs
setupRuntime :: IO ()
setupRuntime = do
  copyFile "./runtime/malgo/runtime.c" (testDirectory </> "libs/runtime.c")

-- | Wrapper of 'Malgo.Driver.compile'
compile :: FilePath -> FilePath -> [FilePath] -> Bool -> Bool -> IO ()
compile src dst modPaths lambdaLift noOptimize = do
  malgoEnv <- newMalgoEnv dst modPaths
  Driver.compile src malgoEnv {lambdaLift, noOptimize}

-- | Get the correct name of `clang`
getClangCommand :: IO String
getClangCommand =
  go ["clang", "clang-12"]
  where
    go [] = error "clang not found"
    go (x : xs) = do
      exitCode <- runProcess (proc "which" [x])
      case exitCode of
        ExitSuccess -> pure x
        ExitFailure _ -> go xs

test :: FilePath -> String -> Bool -> Bool -> IO ()
test testcase postfix lambdaLift noOptimize = do
  compile testcase (testDirectory </> takeBaseName testcase -<.> (postfix <> ".ll")) [testDirectory </> "libs"] lambdaLift noOptimize
  clang <- getClangCommand
  out <- readProcessStdout_ (proc "pkg-config" ["bdw-gc", "--libs", "--cflags"])
  runProcess_
    ( proc
        clang
        [ "-Wno-override-module",
          "-lm",
          chomp $ convertString out,
          testDirectory </> "libs" </> "runtime.c",
          testDirectory </> takeBaseName testcase -<.> (postfix <> ".ll"),
          "-o",
          testDirectory </> takeBaseName testcase -<.> (postfix <> ".out")
        ]
    )

testError :: FilePath -> IO ()
testError testcase = do
  compile testcase (testDirectory </> takeBaseName testcase -<.> ".ll") [testDirectory </> "libs"] False False

testNormal :: FilePath -> IO ()
testNormal testcase = test testcase "" True False

testNoLift :: FilePath -> IO ()
testNoLift testcase = test testcase "nolift" False False

testNoOpt :: FilePath -> IO ()
testNoOpt testcase = test testcase "noopt" True True

testNoNo :: FilePath -> IO ()
testNoNo testcase = test testcase "nono" False True