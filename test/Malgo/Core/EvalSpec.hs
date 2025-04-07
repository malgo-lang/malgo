module Malgo.Core.EvalSpec (spec) where

import Data.ByteString qualified as BS
import Effectful.Error.Static (runError)
import Effectful.Reader.Static (runReader)
import GHC.Exception (CallStack, prettyCallStack)
import Malgo.Core.Eval
import Malgo.Core.Flat qualified as Flat
import Malgo.Desugar.DsState (DsState (..))
import Malgo.Desugar.Pass (desugar)
import Malgo.Driver (failIfError)
import Malgo.Infer.Pass (infer)
import Malgo.Infer.TcEnv (TcEnv (..))
import Malgo.Interface (buildInterface)
import Malgo.Link qualified as Link
import Malgo.Monad (runMalgoM)
import Malgo.Parser (parse)
import Malgo.Prelude
import Malgo.Refine.Pass (refine)
import Malgo.Rename.Pass (rename)
import Malgo.Rename.RnEnv qualified as RnEnv
import Malgo.Rename.RnState (RnState (..))
import Malgo.Syntax
import Malgo.TestUtils
import System.Directory
import System.FilePath
import System.IO.Streams (InputStream, OutputStream)
import System.IO.Streams qualified as Streams
import Test.Hspec

spec :: Spec
spec = parallel do
  runIO do
    setupBuiltin
    setupPrelude
  testcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory testcaseDir
  for_ testcases \testcase -> do
    golden (takeBaseName testcase) do
      result <- driveEval (testcaseDir </> testcase)
      case result of
        Left (cs, err) ->
          error $ prettyCallStack cs <> "\n" <> show err
        Right captured -> pure captured
  errorcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory (testcaseDir </> "error")
  for_ errorcases \errorcase -> do
    it ("error " <> takeBaseName errorcase)
      $ driveEval (testcaseDir </> "error" </> errorcase)
      `shouldThrow` anyException

driveEval :: FilePath -> IO (Either (CallStack, EvalError) String)
driveEval srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag option do
    parsed <-
      parse srcPath src >>= \case
        Left err -> error $ show err
        Right (_, parsed) -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, rnState) <- failIfError <$> rename rnEnv parsed
    (typed, tcEnv) <- infer rnEnv renamed
    refined <- refine tcEnv typed
    (dsState, core) <- desugar tcEnv refined
    core' <- runReader refined.moduleName $ Flat.normalize core
    let inf = buildInterface refined.moduleName rnState tcEnv dsState
    core'' <- Link.link inf core'
    (testStdout, builder) <- setupTestStdout
    runError @EvalError $ runReader refined.moduleName $ do
      eval core'' testStdin testStdout defaultStderr
      liftIO $ readIORef builder

testStdin :: IO (InputStream Char)
testStdin = Streams.fromList "Hello\n"

setupTestStdout :: (MonadIO m) => m (IO (OutputStream Char), IORef String)
setupTestStdout = do
  builder <- newIORef ""
  pure
    ( Streams.makeOutputStream \case
        Just c -> modifyIORef builder (<> [c])
        Nothing -> pure (),
      builder
    )
