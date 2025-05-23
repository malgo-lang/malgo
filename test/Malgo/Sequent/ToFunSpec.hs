module Malgo.Sequent.ToFunSpec (spec) where

import Data.ByteString qualified as BS
import Effectful.Reader.Static (runReader)
import Malgo.Driver (failIfError)
import Malgo.Infer.Pass (infer)
import Malgo.Monad (runMalgoM)
import Malgo.Parser (parse)
import Malgo.Prelude
import Malgo.Refine.Pass (refine)
import Malgo.Rename.Pass (rename)
import Malgo.Rename.RnEnv qualified as RnEnv
import Malgo.SExpr (sShow)
import Malgo.Sequent.ToFun (toFun)
import Malgo.Syntax (Module (..))
import Malgo.TestUtils
import System.Directory
import System.FilePath
import Test.Hspec

spec :: Spec
spec = parallel do
  runIO do
    setupBuiltin
    setupPrelude
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

  golden "Builtin" (driveToFun builtinPath)
  golden "Prelude" (driveToFun preludePath)
  for_ testcases \testcase -> do
    golden (takeBaseName testcase) (driveToFun (testcaseDir </> testcase))

driveToFun :: FilePath -> IO String
driveToFun srcPath = do
  src <- convertString <$> (BS.readFile srcPath)
  runMalgoM flag do
    parsed <-
      parse srcPath src >>= \case
        Left err -> error $ show err
        Right (_, parsed) -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- failIfError <$> rename rnEnv parsed
    (typed, tcEnv, _) <- failIfError <$> infer rnEnv renamed
    refined <- refine tcEnv typed
    program <- runReader refined.moduleName $ toFun refined.moduleDefinition
    pure $ sShow program
