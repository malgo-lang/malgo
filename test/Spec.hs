import Malgo.Prelude
import System.Directory (listDirectory)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath (isExtensionOf, (</>))
import System.Process.Typed
import Test.Hspec

main :: IO ()
main = hspec do
  _ <- runIO $ readProcess (proc "./scripts/pretest.sh" [])
  describe "Test malgo to-ll" do
    testcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory "./testcases/malgo"
    parallel $ for_ testcases \testcase -> do
      it ("run test.sh " <> testcase) $ example do
        (exitCode, out, err) <- readProcess (proc "./scripts/test.sh" ["./testcases/malgo" </> testcase])
        case exitCode of
          ExitSuccess -> pass
          ExitFailure _ -> expectationFailure ("stdout:\n" <> decodeUtf8 out <> "\nstderr:\n" <> decodeUtf8 err)
