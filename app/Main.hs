module Main (main) where

import Data.Map qualified as Map
import Data.Text.IO qualified as T
import Effectful.Error.Static (prettyCallStack, runError)
import Effectful.Log
import Log.Backend.StandardOutput
import Malgo.Core
import Malgo.Core qualified as Core
import Malgo.Eval (EvalError, eval, newEnv)
import Malgo.Eval qualified as Eval
import Malgo.Location (Location (..))
import Malgo.Name
import Malgo.Prelude
import Malgo.Syntax.Parser
import Malgo.Syntax.ResolveName (ResolveError, resolveName)
import Malgo.Syntax.ToCore (ToCoreError, toCore)
import Malgo.Unique
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  _ <- withStdOutLogger \stdOutLogger -> do
    runEff $ runUniqueGen $ runLog "compiler" stdOutLogger LogInfo do
      logInfo_ "Read examples/repeat.mlg"
      readExample "examples/repeat.mlg"
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

readExample :: (UniqueGen :> es, IOE :> es, Log :> es) => FilePath -> Eff es ()
readExample filePath = do
  text <- liftIO $ T.readFile filePath
  defs <- case parse filePath text of
    Left parseErrorBundle -> do
      logAttention_ $ convertString $ errorBundlePretty parseErrorBundle
      pure []
    Right defs -> do
      logInfo_ $ pShow defs
      pure defs
  defs' <- runError @ResolveError $ resolveName defs
  defs'' <- case defs' of
    Left err -> do
      logAttention_ $ pShow err
      pure []
    Right defs' -> do
      logInfo_ $ pShow defs'
      pure defs'
  core <- runError @ToCoreError $ toCore defs''
  core' <- case core of
    Left err -> do
      logAttention_ $ pShow err
      pure []
    Right core -> do
      logInfo_ $ "\n" <> sShow core
      pure core
  core'' <- traverse focusDefinition core'
  let env = newEnv core''
  let mainProcedure = searchMain env
  result <-
    runError @EvalError
      $ eval
        env
        Core.Invoke
          { location =
              Location
                { fileName = filePath,
                  line = 0,
                  column = 0
                },
            name = mainProcedure,
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
  case result of
    Left (callStack, err) -> do
      logAttention_ $ convertString $ prettyCallStack callStack
      logAttention_ $ pShow err
    Right result -> do
      logInfo_ $ pShow result
      pure ()

searchMain :: Eval.Env -> Name
searchMain env = go (Map.keys env.toplevel)
  where
    go [] = error "main procedure not found"
    go (name : names)
      | name.text == "main" = name
      | otherwise = go names
