{-# LANGUAGE Strict #-}

module Malgo.Core.EvalSpec (spec) where

import Data.ByteString qualified as BS
import Effectful.Error.Static (runError)
import Effectful.Reader.Static (runReader)
import GHC.Exception (CallStack, prettyCallStack)
import Malgo.Core.Eval
import Malgo.Core.Flat qualified as Flag
import Malgo.Desugar.Pass (desugar)
import Malgo.Infer.Pass (infer)
import Malgo.Interface (buildInterface)
import Malgo.Link qualified as Link
import Malgo.Monad (CompileMode (..), runMalgoM)
import Malgo.Parser (parseMalgo)
import Malgo.Prelude
import Malgo.Refine.Pass (refine)
import Malgo.Rename.Pass (rename)
import Malgo.Rename.RnEnv qualified as RnEnv
import Malgo.Syntax
import Malgo.TestUtils
import System.Directory
import System.FilePath
import System.IO.Silently (capture)
import Test.Hspec
import Test.Hspec.Golden

spec :: Spec
spec = do
  runIO do
    setupBuiltin
    setupPrelude
  testcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory testcaseDir
  for_ testcases \testcase -> do
    golden ("eval" <> takeBaseName testcase) do
      (captured, result) <- driveEval (testcaseDir </> testcase)
      case result of
        Left (cs, err) ->
          error $ prettyCallStack cs <> "\n" <> show err
        Right value -> pure $ captured <> "\n====\n" <> show value

driveEval :: FilePath -> IO (String, Either (CallStack, EvalError) ())
driveEval srcPath = capture do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM LLVM flag option do
    parsed <-
      parseMalgo srcPath src >>= \case
        Left err -> error $ show err
        Right parsed -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, rnState) <- rename rnEnv parsed
    (typed, tcEnv) <- infer rnEnv renamed
    refined <- refine tcEnv typed
    (dsState, core) <- desugar tcEnv refined
    core' <- runReader refined.moduleName $ Flag.normalize core
    let inf = buildInterface refined.moduleName rnState tcEnv dsState
    core'' <- Link.link inf core'
    runError @EvalError $ runReader refined.moduleName $ eval core''
