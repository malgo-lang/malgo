module Malgo.Sequent.EvalSpec (spec) where

import Data.ByteString qualified as BS
import Effectful
import Effectful.Error.Static (runError)
import Effectful.Reader.Static (runReader)
import Malgo.Driver (failIfError)
import Malgo.Infer.Pass (infer)
import Malgo.Module
import Malgo.Monad (runMalgoM)
import Malgo.Parser (parse)
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
  runMalgoM flag do
    parsed <-
      parse builtinPath src >>= \case
        Left err -> error $ show err
        Right (_, parsed) -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- failIfError <$> rename rnEnv parsed
    (typed, tcEnv) <- failIfError <$> infer rnEnv renamed
    refined <- refine tcEnv typed
    program <- runReader refined.moduleName $ toFun refined.moduleDefinition >>= toCore >>= flatProgram >>= joinProgram
    saveCore refined.moduleName program
    getModulePath refined.moduleName

setupPrelude :: IO ArtifactPath
setupPrelude = do
  src <- convertString <$> BS.readFile preludePath
  runMalgoM flag do
    parsed <-
      parse preludePath src >>= \case
        Left err -> error $ show err
        Right (_, parsed) -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- failIfError <$> rename rnEnv parsed
    (typed, tcEnv) <- failIfError <$> infer rnEnv renamed
    refined <- refine tcEnv typed
    program <- runReader refined.moduleName $ toFun refined.moduleDefinition >>= toCore >>= flatProgram >>= joinProgram
    saveCore refined.moduleName program
    getModulePath refined.moduleName

driveEval :: ArtifactPath -> ArtifactPath -> FilePath -> IO String
driveEval builtinName preludeName srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag do
    parsed <-
      parse srcPath src >>= \case
        Left err -> error $ show err
        Right (_, parsed) -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- failIfError <$> rename rnEnv parsed
    (typed, tcEnv) <- failIfError <$> infer rnEnv renamed
    refined <- refine tcEnv typed
    Program {definitions = program} <- runReader refined.moduleName $ toFun refined.moduleDefinition >>= toCore >>= flatProgram >>= joinProgram

    Program {definitions = builtin} <- load builtinName ".sqt"
    Program {definitions = prelude} <- load preludeName ".sqt"

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
        $ Program {definitions = builtin <> prelude <> program, dependencies = []}
    case result of
      Left (_, err) -> error $ show err
      Right _ -> do
        readIORef stdoutBuilder

saveCore :: (Workspace :> es, IOE :> es) => ModuleName -> Program Join -> Eff es ()
saveCore moduleName program = do
  modulePath <- getModulePath moduleName
  save modulePath ".sqt" program

setupTestStdin :: (MonadIO m) => m (IO (Maybe Char))
setupTestStdin = liftIO do
  ref <- newIORef "Hello\n"
  let stdin :: IO (Maybe Char)
      stdin = do
        str <- readIORef ref
        case str of
          [] -> pure Nothing
          (c : cs) -> do
            writeIORef ref cs
            pure $ Just c
  pure stdin

setupTestStdout :: (MonadIO m) => m (Char -> IO (), IORef String)
setupTestStdout = do
  builder <- newIORef ""
  let stdout = \c -> modifyIORef builder (<> [c])
  pure (stdout, builder)

setupTestStderr :: (MonadIO m) => m (Char -> IO (), IORef String)
setupTestStderr = do
  builder <- newIORef ""
  let stderr = \c -> modifyIORef builder (<> [c])
  pure (stderr, builder)