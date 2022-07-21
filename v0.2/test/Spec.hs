{-# LANGUAGE CPP #-}

import Malgo.Prelude
import System.Directory (listDirectory)
import System.FilePath (isExtensionOf, (</>))
import System.Process.Typed
import Test.Hspec

-- | 'stack build' and 'stack test' pass '-DSTACK' to ghc.
-- This enables switching test scripts mode 'stack' or 'cabal'.
buildCommand :: String
#ifdef STACK
buildCommand = "stack"
#else
buildCommand = "cabal"
#endif

main :: IO ()
main = do
  hspec do
    _ <- runIO $ readProcess (proc "./scripts/pretest.sh" [buildCommand])
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
