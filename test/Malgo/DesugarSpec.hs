module Malgo.DesugarSpec (spec) where

import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Malgo.Desugar.Pass (desugar)
import Malgo.Infer.Pass (infer)
import Malgo.Interface (buildInterface)
import Malgo.Link qualified as Link
import Malgo.Module (Resource (..), ViaStore (..), parseArtifactPath, pwdPath)
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
import Test.Hspec

spec :: Spec
spec = do
  testcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory testcaseDir
  for_ testcases \testcase -> do
    goldenJSON "desugar" (takeBaseName testcase) (driveDesugar (testcaseDir </> testcase))

driveDesugar :: FilePath -> IO BL.ByteString
driveDesugar srcPath = do
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
    (dsEnv, core) <- desugar tcEnv refined

    pwd <- pwdPath
    srcPath <- parseArtifactPath pwd srcPath
    save srcPath ".mo" (ViaStore core)

    let inf = buildInterface refined.moduleName rnState tcEnv dsEnv
    save srcPath ".mlgi" (ViaStore inf)

    core <- Link.link inf core

    pure $ Aeson.encodePretty core
