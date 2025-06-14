module Malgo.InferSpec (spec) where

import Data.ByteString qualified as BS
import Effectful.Error.Static (catchError)
import Malgo.Infer
import Malgo.Monad (runMalgoM)
import Malgo.Parser.Pass
import Malgo.Pass
import Malgo.Prelude
import Malgo.Rename
import Malgo.TestUtils
import System.Directory
import System.FilePath
import Test.Hspec

errorcaseDir :: FilePath
errorcaseDir = "test/Malgo/InferSpec/errors"

spec :: Spec
spec = parallel do
  runIO do
    setupBuiltin
    setupPrelude
  testcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory testcaseDir
  golden "Builtin" (driveInfer builtinPath)
  golden "Prelude" (driveInfer preludePath)
  for_ testcases \testcase -> do
    golden (takeBaseName testcase) (driveInfer (testcaseDir </> testcase))
  errorcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory errorcaseDir
  for_ errorcases \errorcase -> do
    golden ("error " <> takeBaseName errorcase) (driveErrorInfer (errorcaseDir </> errorcase))

driveInfer :: FilePath -> IO String
driveInfer srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag $ runCompileError do
    parsed <- runPass ParserPass (srcPath, src)
    rnEnv <- genBuiltinRnEnv
    (renamed, _) <- runPass RenamePass (parsed, rnEnv)
    (typedAst, _, _) <- runPass InferPass (renamed, rnEnv)
    pure $ pShowCompact typedAst

driveErrorInfer :: FilePath -> IO String
driveErrorInfer srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag $ runCompileError do
    parsed <- runPass ParserPass (srcPath, src)
    rnEnv <- genBuiltinRnEnv
    (renamed, _) <- runPass RenamePass (parsed, rnEnv)
    fmap show (runPass InferPass (renamed, rnEnv)) `catchError` \_ CompileError {compileError} -> pure $ show compileError
