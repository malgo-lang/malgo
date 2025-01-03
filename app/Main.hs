module Main (main) where

import Data.Text.IO qualified as T
import Effectful.Error.Static (prettyCallStack, runError)
import Effectful.Log
import Log.Backend.StandardOutput
import Malgo.Core
import Malgo.Core.Builder
import Malgo.Eval (EvalError, eval, newEnv)
import Malgo.Parser
import Malgo.Prelude
import Malgo.Unique
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  _ <- withStdOutLogger \stdOutLogger -> do
    runEff $ runUniqueGen $ runLog "compiler" stdOutLogger LogInfo do
      -- logInfo_ "Run ex1"
      -- runExample ex1
      -- logInfo_ "Run ex2"
      -- runExample ex2
      logInfo_ "Read examples/mult.mlg"
      readExample "examples/mult.mlg"
  pure ()

runExample :: (UniqueGen :> es, Log :> es) => Eff es (Statement, [Definition]) -> Eff es ()
runExample example = do
  (stmt, defs) <- example
  stmt' <- focus stmt
  defs' <- traverse focusDefinition defs
  let env = newEnv defs'
  result <- runError @EvalError $ eval env stmt'
  case result of
    Left (callStack, err) -> do
      logAttention_ $ convertString $ prettyCallStack callStack
      logAttention_ $ pShow err
    Right _ -> pure ()

readExample :: (MonadIO m, MonadLog m) => FilePath -> m ()
readExample filePath = do
  text <- liftIO $ T.readFile filePath
  case parse filePath text of
    Left parseErrorBundle -> do
      logAttention_ $ convertString $ errorBundlePretty parseErrorBundle
    Right defs -> do
      logInfo_ $ pShow defs