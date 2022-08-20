{-# LANGUAGE CPP #-}

import Malgo.Prelude
import System.Directory (listDirectory)
import System.FilePath (isExtensionOf, (</>))
import System.Process.Typed
import Test.Hspec

-- | 'stack build' and 'stack test' pass '-DSTACK' to ghc.
-- This enables switching test scripts mode 'stack' or 'cabal'.
buildCommand :: String
buildCommand = "cabal"

main :: IO ()
main =
  hspec do
    (exitCode, _, stderr) <- runIO $ readProcess (proc "./scripts/pretest.sh" [buildCommand])
    case exitCode of
      ExitSuccess -> pass
      ExitFailure _ -> error $ "pretest.sh failed\n" <> decodeUtf8 stderr
    testcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory "./testcases/malgo"
    describe "Test malgo to-ll" do
      parallel $ for_ testcases \testcase -> do
        it ("test usual case " <> testcase) $ example do
          (exitCode, out, err) <- readProcess (proc "./scripts/test/test.sh" ["./testcases/malgo" </> testcase, buildCommand])
          case exitCode of
            ExitSuccess -> pass
            ExitFailure _ -> expectationFailure ("stdout:\n" <> decodeUtf8 out <> "\nstderr:\n" <> decodeUtf8 err)

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
