import Data.List (intercalate, isSuffixOf)
import Data.Map qualified as Map
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as TL
import Effectful.Error.Static (runError)
import Effectful.Log (LogLevel (LogInfo), runLog)
import Effectful.Writer.Static.Local (execWriter, tell)
import Log.Backend.StandardOutput (withStdOutLogger)
import Malgo.Core qualified as Core
import Malgo.Eval qualified as Eval
import Malgo.Location (Location (..))
import Malgo.Name
import Malgo.Prelude
import Malgo.Surface.Convert qualified as Surface
import Malgo.Surface.Parser qualified as Surface
import Malgo.Syntax.Parser qualified as Syntax
import Malgo.Syntax.ResolveName qualified as Syntax
import Malgo.Syntax.ToCore qualified as Syntax
import Malgo.Unique (runUniqueGen)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.Core.Spec (getSpecDescriptionPath)
import Test.Hspec.Golden (Golden (..))
import Text.Megaparsec (errorBundlePretty)
import Text.Pretty.Simple (pShowNoColor)

main :: IO ()
main = do
  exampleFilePaths <- listDirectory "examples"
  let irFilePaths = map ("examples" </>) $ filter (".ir" `isSuffixOf`) exampleFilePaths
  let mlgFilePaths = map ("examples" </>) $ filter (".mlg" `isSuffixOf`) exampleFilePaths
  irGoldenTests <- traverse buildIrGoldenTest irFilePaths
  mlgGoldenTests <- traverse buildMlgGoldenTest mlgFilePaths
  hspec do
    describe "IrGoldenTests" do
      sequence_ irGoldenTests
    describe "MlgGoldenTests" do
      sequence_ mlgGoldenTests

buildIrGoldenTest :: FilePath -> IO Spec
buildIrGoldenTest path = withStdOutLogger \stdOutLogger -> do
  specs <- runEff $ execWriter @[Spec] $ runUniqueGen $ runLog "irGoldenTest" stdOutLogger LogInfo do
    src <- liftIO $ T.readFile path
    parsed <- case Syntax.parse path src of
      Left bundle -> error $ errorBundlePretty bundle
      Right parsed -> pure parsed
    tell [golden ("Parse " <> path) parsed]
    resolved <-
      runError @Syntax.ResolveError (Syntax.resolveName parsed)
        >>= \case
          Left err -> error $ convertString $ pShowNoColor err
          Right resolved -> pure resolved
    tell [golden ("ResolveName " <> path) resolved]
    core <-
      runError @Syntax.ToCoreError (Syntax.toCore resolved)
        >>= \case
          Left err -> error $ convertString $ pShowNoColor err
          Right core -> pure core
    tell [golden ("ToCore " <> path) core]
    focused <- traverse Core.focusDefinition core
    tell [golden ("Focus " <> path) focused]
    let env = Eval.newEnv focused
    let mainProcedure = searchMain path env
    result <-
      runError @Eval.EvalError (Eval.eval env mainProcedure)
        >>= \case
          Left err -> error $ convertString $ pShowNoColor err
          Right result -> pure result
    tell [golden ("Eval " <> path) result]
  pure $ sequence_ specs

searchMain :: FilePath -> Eval.Env -> Core.Statement
searchMain filePath env = go (Map.keys env.toplevel)
  where
    go [] = error "main procedure not found"
    go (name : names)
      | name.text == "main" =
          Core.Invoke
            { location =
                Location
                  { fileName = filePath,
                    line = 0,
                    column = 0
                  },
              name = name,
              producers = [],
              consumers =
                [ Core.Finish
                    { location =
                        Location
                          { fileName = filePath,
                            line = 0,
                            column = 0
                          }
                    }
                ]
            }
      | otherwise = go names

buildMlgGoldenTest :: FilePath -> IO Spec
buildMlgGoldenTest path = withStdOutLogger \stdOutLogger -> do
  specs <- runEff $ execWriter @[Spec] $ runUniqueGen $ runLog "mlgGoldenTest" stdOutLogger LogInfo do
    src <- liftIO $ T.readFile path
    parsed <- case Surface.parse path src of
      Left bundle -> error $ errorBundlePretty bundle
      Right parsed -> pure parsed
    tell [golden ("Parse " <> path) parsed]
    let converted = case Surface.toSyntax parsed of
          Left err -> error $ convertString $ pShowNoColor err
          Right converted -> converted
    resolved <-
      runError @Syntax.ResolveError (Syntax.resolveName converted)
        >>= \case
          Left err -> error $ convertString $ pShowNoColor err
          Right resolved -> pure resolved
    tell [golden ("ResolveName " <> path) resolved]
    core <-
      runError @Syntax.ToCoreError (Syntax.toCore resolved)
        >>= \case
          Left err -> error $ convertString $ pShowNoColor err
          Right core -> pure core
    tell [golden ("ToCore " <> path) core]
    focused <- traverse Core.focusDefinition core
    tell [golden ("Focus " <> path) focused]
    let env = Eval.newEnv focused
    let mainProcedure = searchMain path env
    result <-
      runError @Eval.EvalError (Eval.eval env mainProcedure)
        >>= \case
          Left err -> error $ convertString $ pShowNoColor err
          Right result -> pure result
    tell [golden ("Eval " <> path) result]
  pure $ sequence_ specs

golden :: (Show a) => String -> a -> Spec
golden description actual = do
  name <- intercalate "-" . (<> words description) <$> getSpecDescriptionPath
  it
    description
    Golden
      { output = pShowNoColor actual,
        encodePretty = convertString,
        writeToFile = TL.writeFile,
        readFromFile = TL.readFile,
        goldenFile = ".golden" </> name </> "golden",
        actualFile = Just $ ".golden" </> name </> "actual",
        failFirstTime = False
      }