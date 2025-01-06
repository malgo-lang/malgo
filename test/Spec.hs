import Data.Map qualified as Map
import Data.Text.IO qualified as T
import Effectful.Error.Static (prettyCallStack, runError)
import Effectful.Log
import Log.Backend.StandardOutput
import Malgo.Core (focusDefinition)
import Malgo.Core qualified as Core
import Malgo.Eval (EvalError)
import Malgo.Eval qualified as Eval
import Malgo.Location (Location (..))
import Malgo.Name (Name (..))
import Malgo.Parser
import Malgo.Prelude
import Malgo.Syntax.ResolveName
import Malgo.Syntax.ToCore
import Malgo.Unique
import Test.Hspec (describe, hspec)
import Test.Hspec.Golden (golden)
import Text.Megaparsec (errorBundlePretty)
import Text.Pretty.Simple (OutputOptions (..), defaultOutputOptionsNoColor, pShowOpt)

main :: IO ()
main = hspec do
  describe "ParserGoldenTests" do
    golden ("parser examples/mult.mlg") $ parseFile "examples/mult.mlg"
    golden ("parser examples/mult0.mlg") $ parseFile "examples/mult0.mlg"
  describe "ResolveNameGoldenTests" do
    golden ("resolveName examples/mult.mlg") $ resolveNameFile "examples/mult.mlg"
    golden ("resolveName examples/mult0.mlg") $ resolveNameFile "examples/mult0.mlg"
  describe "ToCoreGoldenTests" do
    golden ("toCore examples/mult.mlg") $ toCoreFile "examples/mult.mlg"
    golden ("toCore examples/mult0.mlg") $ toCoreFile "examples/mult0.mlg"
  describe "FocusGoldenTests" do
    golden ("focus examples/mult.mlg") $ focusFile "examples/mult.mlg"
    golden ("focus examples/mult0.mlg") $ focusFile "examples/mult0.mlg"
  describe "EvalGoldenTests" do
    golden ("eval examples/mult.mlg") $ evalFile "examples/mult.mlg"
    golden ("eval examples/mult0.mlg") $ evalFile "examples/mult0.mlg"

parseFile :: FilePath -> IO String
parseFile path = do
  src <- T.readFile path
  case parse path src of
    Left bundle -> error $ errorBundlePretty bundle
    Right defs -> pure $ convertString $ pShowOpt smallIndentNoColor defs

resolveNameFile :: FilePath -> IO String
resolveNameFile path = withStdOutLogger \stdOutLogger ->
  runEff $ runUniqueGen $ runLog "resolveNameTest" stdOutLogger LogInfo do
    src <- liftIO $ T.readFile path
    defs <- case parse path src of
      Left bundle -> error $ errorBundlePretty bundle
      Right defs -> pure defs
    defs' <- runError @ResolveError $ resolveName defs
    case defs' of
      Left err -> error $ pShow err
      Right defs' -> pure $ convertString $ pShowOpt smallIndentNoColor defs'

toCoreFile :: FilePath -> IO String
toCoreFile path = withStdOutLogger \stdOutLogger ->
  runEff $ runUniqueGen $ runLog "resolveNameTest" stdOutLogger LogInfo do
    src <- liftIO $ T.readFile path
    defs <- case parse path src of
      Left bundle -> error $ errorBundlePretty bundle
      Right defs -> pure defs
    defs' <-
      runError @ResolveError (resolveName defs)
        >>= \case
          Left err -> error $ pShow err
          Right defs' -> pure defs'
    core <-
      runError @ToCoreError (toCore defs')
        >>= \case
          Left err -> error $ pShow err
          Right core -> pure core
    pure $ convertString $ pShowOpt smallIndentNoColor core

focusFile :: FilePath -> IO String
focusFile path = withStdOutLogger \stdOutLogger ->
  runEff $ runUniqueGen $ runLog "resolveNameTest" stdOutLogger LogInfo do
    src <- liftIO $ T.readFile path
    defs <- case parse path src of
      Left bundle -> error $ errorBundlePretty bundle
      Right defs -> pure defs
    defs' <-
      runError @ResolveError (resolveName defs)
        >>= \case
          Left err -> error $ pShow err
          Right defs' -> pure defs'
    core <-
      runError @ToCoreError (toCore defs')
        >>= \case
          Left err -> error $ pShow err
          Right core -> pure core
    core' <- traverse focusDefinition core
    pure $ convertString $ pShowOpt smallIndentNoColor core'

evalFile :: FilePath -> IO String
evalFile path = withStdOutLogger \stdOutLogger ->
  runEff $ runUniqueGen $ runLog "resolveNameTest" stdOutLogger LogInfo do
    src <- liftIO $ T.readFile path
    defs <- case parse path src of
      Left bundle -> error $ errorBundlePretty bundle
      Right defs -> pure defs
    defs' <-
      runError @ResolveError (resolveName defs)
        >>= \case
          Left err -> error $ pShow err
          Right defs' -> pure defs'
    core <-
      runError @ToCoreError (toCore defs')
        >>= \case
          Left err -> error $ pShow err
          Right core -> pure core
    core' <- traverse focusDefinition core
    let env = Eval.newEnv core'
    let mainProcedure = searchMain env
    result <-
      runError @EvalError
        $ Eval.eval
          env
          Core.Invoke
            { location =
                Location
                  { fileName = path,
                    line = 0,
                    column = 0
                  },
              name = mainProcedure,
              producers = [],
              consumers =
                [ Core.Finish
                    { location =
                        Location
                          { fileName = path,
                            line = 0,
                            column = 0
                          }
                    }
                ]
            }
    case result of
      Left (callStack, err) -> do
        logAttention_ $ convertString $ prettyCallStack callStack
        error $ pShow err
      Right result -> pure $ convertString $ pShowOpt smallIndentNoColor result

searchMain :: Eval.Env -> Name
searchMain env = go (Map.keys env.toplevel)
  where
    go [] = error "main procedure not found"
    go (name : names)
      | name.text == "main" = name
      | otherwise = go names

smallIndentNoColor :: OutputOptions
smallIndentNoColor =
  defaultOutputOptionsNoColor
    { outputOptionsIndentAmount = 1,
      outputOptionsCompactParens = True,
      outputOptionsCompact = True
    }