module Malgo.InferSpec (spec) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Malgo.Infer.Pass (infer)
import Malgo.Monad (CompileMode (..), runMalgoM)
import Malgo.Parser (parseMalgo)
import Malgo.Prelude
import Malgo.Rename.Pass (rename)
import Malgo.Rename.RnEnv qualified as RnEnv
import Malgo.TestUtils
import System.Directory
import System.FilePath
import Test.Hspec

spec :: Spec
spec = do
  testcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory testcaseDir
  for_ testcases \testcase -> do
    goldenHaskell "infer" (takeBaseName testcase) (driveInfer (testcaseDir </> testcase))

driveInfer :: FilePath -> IO BL.ByteString
driveInfer srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM LLVM flag option do
    parsed <-
      parseMalgo srcPath src >>= \case
        Left err -> error $ show err
        Right parsed -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- rename rnEnv parsed
    (typedAst, _) <- infer rnEnv renamed
    pure $ pShowCompact typedAst