module Malgo.Sequent.ToCoreSpec (spec) where

import Data.ByteString qualified as BS
import Effectful.Reader.Static (runReader)
import Malgo.Driver (failIfError)
import Malgo.Infer.Pass (infer)
import Malgo.Monad (runMalgoM)
import Malgo.Parser (parseMalgo)
import Malgo.Prelude
import Malgo.Refine.Pass (refine)
import Malgo.Rename.Pass (rename)
import Malgo.Rename.RnEnv qualified as RnEnv
import Malgo.SExpr (sShow)
import Malgo.Sequent.Core.Flat (flatProgram)
import Malgo.Sequent.Core.Join (joinProgram)
import Malgo.Sequent.ToCore (toCore)
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

  golden "Builtin" (driveToCore builtinPath)
  golden "Builtin flat" (driveFlat builtinPath)
  golden "Builtin join" (driveJoin builtinPath)
  golden "Prelude" (driveToCore preludePath)
  golden "Prelude flat" (driveFlat builtinPath)
  golden "Prelude join" (driveJoin preludePath)
  for_ testcases \testcase -> do
    golden (takeBaseName testcase) (driveToCore (testcaseDir </> testcase))
    golden (takeBaseName testcase <> " flat") (driveFlat (testcaseDir </> testcase))
    golden (takeBaseName testcase <> " join") (driveJoin (testcaseDir </> testcase))

driveToCore :: FilePath -> IO String
driveToCore srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag option do
    parsed <-
      parseMalgo srcPath src >>= \case
        Left err -> error $ show err
        Right parsed -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- failIfError <$> rename rnEnv parsed
    (typed, tcEnv) <- infer rnEnv renamed
    refined <- refine tcEnv typed
    program <- runReader refined.moduleName $ toFun refined.moduleDefinition >>= toCore
    pure $ sShow program

driveFlat :: FilePath -> IO String
driveFlat srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag option do
    parsed <-
      parseMalgo srcPath src >>= \case
        Left err -> error $ show err
        Right parsed -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- failIfError <$> rename rnEnv parsed
    (typed, tcEnv) <- infer rnEnv renamed
    refined <- refine tcEnv typed
    program <- runReader refined.moduleName $ toFun refined.moduleDefinition >>= toCore >>= flatProgram
    pure $ sShow program

driveJoin :: FilePath -> IO String
driveJoin srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag option do
    parsed <-
      parseMalgo srcPath src >>= \case
        Left err -> error $ show err
        Right parsed -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- failIfError <$> rename rnEnv parsed
    (typed, tcEnv) <- infer rnEnv renamed
    refined <- refine tcEnv typed
    program <- runReader refined.moduleName $ toFun refined.moduleDefinition >>= toCore >>= flatProgram >>= joinProgram
    pure $ sShow program