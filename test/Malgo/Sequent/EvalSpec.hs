module Malgo.Sequent.EvalSpec (spec) where

import Data.ByteString qualified as BS
import Effectful
import Effectful.Error.Static (runError)
import Effectful.Reader.Static (runReader)
import Malgo.Module
import Malgo.Monad (runMalgoM)
import Malgo.Parser (parse)
import Malgo.Parser.Pass
import Malgo.Pass (runCompileError, runPass)
import Malgo.Prelude
import Malgo.Rename
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
    pure $ filter (isExtensionOf "mlg") files

  for_ testcases \testcase -> do
    golden (takeBaseName testcase) (driveEval builtin prelude (testcaseDir </> testcase))

setupBuiltin :: IO ArtifactPath
setupBuiltin = do
  src <- convertString <$> BS.readFile builtinPath
  runMalgoM flag $ runCompileError do
    parsed <- runPass ParserPass (builtinPath, src)
    rnEnv <- genBuiltinRnEnv
    (renamed, _) <- runPass RenamePass (parsed, rnEnv)
    program <- runReader renamed.moduleName $ toFun renamed.moduleDefinition >>= toCore >>= flatProgram >>= joinProgram
    saveCore renamed.moduleName program
    getModulePath renamed.moduleName

setupPrelude :: IO ArtifactPath
setupPrelude = do
  src <- convertString <$> BS.readFile preludePath
  runMalgoM flag $ runCompileError do
    parsed <- runPass ParserPass (preludePath, src)
    rnEnv <- genBuiltinRnEnv
    (renamed, _) <- runPass RenamePass (parsed, rnEnv)
    program <- runReader renamed.moduleName $ toFun renamed.moduleDefinition >>= toCore >>= flatProgram >>= joinProgram
    saveCore renamed.moduleName program
    getModulePath renamed.moduleName

driveEval :: ArtifactPath -> ArtifactPath -> FilePath -> IO String
driveEval builtinName preludeName srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag $ runCompileError do
    parsed <-
      parse srcPath src >>= \case
        Left err -> error $ show err
        Right parsed -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- runPass RenamePass (parsed, rnEnv)
    Program {definitions = program} <- runReader renamed.moduleName $ toFun renamed.moduleDefinition >>= toCore >>= flatProgram >>= joinProgram

    Program {definitions = builtin} <- load builtinName ".sqt"
    Program {definitions = prelude} <- load preludeName ".sqt"

    stdin <- setupTestStdin
    (stdout, stdoutBuilder) <- setupTestStdout
    (stderr, _) <- setupTestStderr

    result <-
      runError @EvalError
        $ runReader renamed.moduleName
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
  let stdout c = modifyIORef builder (<> [c])
  pure (stdout, builder)

setupTestStderr :: (MonadIO m) => m (Char -> IO (), IORef String)
setupTestStderr = do
  builder <- newIORef ""
  let stderr c = modifyIORef builder (<> [c])
  pure (stderr, builder)
