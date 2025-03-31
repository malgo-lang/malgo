module Malgo.Sequent.EvalSpec (spec) where

import Data.ByteString qualified as BS
import Effectful
import Effectful.Error.Static (runError)
import Effectful.Reader.Static (runReader)
import Malgo.Driver (failIfError)
import Malgo.Infer.Pass (infer)
import Malgo.Module
import Malgo.Monad (runMalgoM)
import Malgo.NewParser (parse)
import Malgo.NewRename.Pass (rename)
import Malgo.NewRename.RnEnv qualified as RnEnv
import Malgo.Prelude
import Malgo.Refine.Pass (refine)
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
  testcases <- runIO do
    files <- listDirectory testcaseDir
    let mlgFiles = filter (isExtensionOf "mlg") files
    -- Filter out files that start with "-- backend: core"
    -- These files are for the core backend and are not supported by the sequent backend
    filterM
      ( \file -> do
          contents <- BS.readFile (testcaseDir </> file)
          pure $ not $ "#backend core" `BS.isPrefixOf` contents
      )
      mlgFiles

  for_ testcases \testcase -> do
    golden (takeBaseName testcase) (driveEval builtin prelude (testcaseDir </> testcase))

setupBuiltin :: IO ArtifactPath
setupBuiltin = do
  src <- convertString <$> BS.readFile builtinPath
  runMalgoM flag option do
    parsed <-
      parse builtinPath src >>= \case
        Left err -> error $ show err
        Right (_, parsed) -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- failIfError <$> rename rnEnv parsed
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
      parse preludePath src >>= \case
        Left err -> error $ show err
        Right (_, parsed) -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- failIfError <$> rename rnEnv parsed
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
      parse srcPath src >>= \case
        Left err -> error $ show err
        Right (_, parsed) -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- failIfError <$> rename rnEnv parsed
    (typed, tcEnv) <- infer rnEnv renamed
    refined <- refine tcEnv typed
    Program program <- runReader refined.moduleName $ toFun refined.moduleDefinition >>= toCore >>= flatProgram >>= joinProgram

    Program builtin <- load builtinName ".sqt"
    Program prelude <- load preludeName ".sqt"

    stdin <- setupTestStdin
    (stdout, stdoutBuilder) <- setupTestStdout
    (stderr, _) <- setupTestStderr

    result <-
      runError @EvalError
        $ runReader refined.moduleName
        $ runReader
          Handlers
            { stdin,
              stdout,
              stderr
            }
        $ evalProgram
        $ Program (builtin <> prelude <> program)
    case result of
      Left (_, err) -> error $ show err
      Right _ -> do
        readIORef stdoutBuilder

saveCore :: (Workspace :> es, IOE :> es) => ModuleName -> Program Join -> Eff es ()
saveCore moduleName program = do
  modulePath <- getModulePath moduleName
  save modulePath ".sqt" program

setupTestStdin :: (MonadIO m) => m (InputStream Char)
setupTestStdin = liftIO $ Streams.fromList "Hello\n"

setupTestStdout :: (MonadIO m) => m (OutputStream Char, IORef String)
setupTestStdout = do
  builder <- newIORef ""
  stdout <- liftIO $ Streams.makeOutputStream \case
    Just c -> modifyIORef builder (<> [c])
    Nothing -> pure ()
  pure
    ( stdout,
      builder
    )

setupTestStderr :: (MonadIO m) => m (OutputStream Char, IORef String)
setupTestStderr = do
  builder <- newIORef ""
  stderr <- liftIO $ Streams.makeOutputStream \case
    Just c -> modifyIORef builder (<> [c])
    Nothing -> pure ()
  pure
    ( stderr,
      builder
    )