module Malgo.Sequent.EvalSpec (spec) where

import Data.ByteString qualified as BS
import Effectful
import Effectful.Error.Static (runError)
import Effectful.Reader.Static (runReader)
import Malgo.Infer.Pass (infer)
import Malgo.Module
import Malgo.Monad (runMalgoM)
import Malgo.Parser (parseMalgo)
import Malgo.Prelude
import Malgo.Refine.Pass (refine)
import Malgo.Rename.Pass (rename)
import Malgo.Rename.RnEnv qualified as RnEnv
import Malgo.Sequent.Core
import Malgo.Sequent.Core.Flat (flatProgram)
import Malgo.Sequent.Core.Join (joinProgram)
import Malgo.Sequent.Eval (EvalError, Handlers (..), evalProgram)
import Malgo.Sequent.ToCore (toCore)
import Malgo.Sequent.ToFun (toFun)
import Malgo.Syntax (Module (..))
import Malgo.TestUtils hiding (setupBuiltin, setupPrelude)
import System.Directory
import System.FilePath
import System.IO.Streams (InputStream, OutputStream)
import System.IO.Streams qualified as Streams
import Test.Hspec

spec :: Spec
spec = parallel do
  (builtin, prelude) <- runIO do
    builtin <- setupBuiltin
    prelude <- setupPrelude
    pure (builtin, prelude)
  testcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory testcaseDir
  for_ testcases \testcase -> do
    golden (takeBaseName testcase) (driveEval builtin prelude (testcaseDir </> testcase))

setupBuiltin :: IO ArtifactPath
setupBuiltin = do
  src <- convertString <$> BS.readFile builtinPath
  runMalgoM flag option do
    parsed <-
      parseMalgo builtinPath src >>= \case
        Left err -> error $ show err
        Right parsed -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- rename rnEnv parsed
    (typed, tcEnv) <- infer rnEnv renamed
    refined <- refine tcEnv typed
    program <- runReader refined.moduleName $ toFun refined.moduleDefinition >>= toCore >>= flatProgram >>= joinProgram
    saveCore refined.moduleName program
    getModulePath refined.moduleName

setupPrelude :: IO ArtifactPath
setupPrelude = do
  src <- convertString <$> BS.readFile preludePath
  runMalgoM flag option do
    parsed <-
      parseMalgo preludePath src >>= \case
        Left err -> error $ show err
        Right parsed -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- rename rnEnv parsed
    (typed, tcEnv) <- infer rnEnv renamed
    refined <- refine tcEnv typed
    program <- runReader refined.moduleName $ toFun refined.moduleDefinition >>= toCore >>= flatProgram >>= joinProgram
    saveCore refined.moduleName program
    getModulePath refined.moduleName

driveEval :: ArtifactPath -> ArtifactPath -> FilePath -> IO String
driveEval builtinName preludeName srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag option do
    parsed <-
      parseMalgo srcPath src >>= \case
        Left err -> error $ show err
        Right parsed -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- rename rnEnv parsed
    (typed, tcEnv) <- infer rnEnv renamed
    refined <- refine tcEnv typed
    Program program <- runReader refined.moduleName $ toFun refined.moduleDefinition >>= toCore >>= flatProgram >>= joinProgram

    Program builtin <- load builtinName ".sqt"
    Program prelude <- load preludeName ".sqt"

    (stdout, stdoutBuilder) <- setupTestStdout
    (stderr, _) <- setupTestStderr

    result <-
      runError @EvalError
        $ runReader refined.moduleName
        $ runReader
          Handlers
            { stdin = testStdin,
              stdout,
              stderr
            }
        $ evalProgram
        $ Program (builtin <> prelude <> program)
    case result of
      Left (_, err) -> error $ show err
      Right _ -> do
        stdout <- readIORef stdoutBuilder
        pure stdout

saveCore :: (Workspace :> es, IOE :> es) => ModuleName -> Program Join -> Eff es ()
saveCore moduleName program = do
  modulePath <- getModulePath moduleName
  save modulePath ".sqt" program

testStdin :: (MonadIO m) => m (InputStream Char)
testStdin = liftIO $ Streams.fromList "Hello\n"

setupTestStdout :: (MonadIO m) => m (IO (OutputStream Char), IORef String)
setupTestStdout = do
  builder <- newIORef ""
  pure
    ( Streams.makeOutputStream \case
        Just c -> modifyIORef builder (<> [c])
        Nothing -> pure (),
      builder
    )

setupTestStderr :: (MonadIO m) => m (IO (OutputStream Char), IORef String)
setupTestStderr = do
  builder <- newIORef ""
  pure
    ( Streams.makeOutputStream \case
        Just c -> modifyIORef builder (<> [c])
        Nothing -> pure (),
      builder
    )