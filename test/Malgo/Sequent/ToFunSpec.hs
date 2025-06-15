module Malgo.Sequent.ToFunSpec (spec) where

import Data.ByteString qualified as BS
import Effectful.Reader.Static (runReader)
import Malgo.Monad (runMalgoM)
import Malgo.Parser.Pass
import Malgo.Pass
import Malgo.Prelude
import Malgo.Rename
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
    pure $ filter (isExtensionOf "mlg") files

  golden "Builtin" (driveToFun builtinPath)
  golden "Prelude" (driveToFun preludePath)
  for_ testcases \testcase -> do
    golden (takeBaseName testcase) (driveToFun (testcaseDir </> testcase))

driveToFun :: FilePath -> IO String
driveToFun srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag $ runCompileError do
    parsed <- runPass ParserPass (srcPath, src)
    rnEnv <- genBuiltinRnEnv
    (renamed, _) <- runPass RenamePass (parsed, rnEnv)
    program <- runReader renamed.moduleName $ toFun renamed.moduleDefinition
    pure $ sShow program
