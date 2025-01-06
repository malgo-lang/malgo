import Data.Text.IO qualified as T
import Effectful.Error.Static (runError)
import Effectful.Log
import Log.Backend.StandardOutput
import Malgo.Core (focusDefinition)
import Malgo.Parser
import Malgo.Prelude
import Malgo.Syntax.ResolveName
import Malgo.Syntax.ToCore
import Malgo.Unique
import Test.Hspec ( hspec, describe )
import Test.Hspec.Golden ( golden )
import Text.Megaparsec (errorBundlePretty)
import Text.Pretty.Simple (OutputOptions (..), defaultOutputOptionsNoColor, pShowOpt)

main :: IO ()
main = hspec do
  describe "ParserGoldenTests" do
    golden ("parser examples/mult.mlg") $ parseFile "examples/mult.mlg"
  describe "ResolveNameGoldenTests" do
    golden ("resolveName examples/mult.mlg") $ resolveNameFile "examples/mult.mlg"
  describe "ToCoreGoldenTests" do
    golden ("toCore examples/mult.mlg") $ toCoreFile "examples/mult.mlg"
  describe "FocusGoldenTests" do
    golden ("focus examples/mult.mlg") $ focusFile "examples/mult.mlg"

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

smallIndentNoColor :: OutputOptions
smallIndentNoColor =
  defaultOutputOptionsNoColor
    { outputOptionsIndentAmount = 1,
      outputOptionsCompactParens = True,
      outputOptionsCompact = True
    }