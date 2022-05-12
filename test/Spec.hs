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
main = hspec do
  _ <- runIO $ readProcess (proc "./scripts/pretest.sh" [buildCommand])
  describe "Test malgo to-ll" do
    testcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory "./testcases/malgo"
    parallel $ for_ testcases \testcase -> do
      it ("run test.sh " <> testcase) $ example do
        (exitCode, out, err) <- readProcess (proc "./scripts/test.sh" ["./testcases/malgo" </> testcase, buildCommand])
        case exitCode of
          ExitSuccess -> pass
          ExitFailure _ -> expectationFailure ("stdout:\n" <> decodeUtf8 out <> "\nstderr:\n" <> decodeUtf8 err)
