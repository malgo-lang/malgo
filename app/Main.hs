module Main (main) where

import Data.Foldable (for_)
import Data.Map qualified as Map
import Data.Text.IO qualified as T
import Effectful.Environment (getArgs, runEnvironment)
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
import Malgo.Surface.Parser qualified as Surface
import Malgo.Syntax.Parser
import Malgo.Syntax.ResolveName (ResolveError, resolveName)
import Malgo.Syntax.ToCore (ToCoreError, toCore)
import Malgo.Unique
import System.FilePath (takeExtension)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  _ <- withStdOutLogger \stdOutLogger -> do
    runEff $ runEnvironment $ runUniqueGen $ runLog "compiler" stdOutLogger LogInfo do
      -- Read command-line arguments
      args <- getArgs
      -- take the first argument as a file path
      filePath <- case args of
        [filePath] -> pure filePath
        _ -> do
          logAttention_ "Usage: malgo-exe <file-path>"
          error "Invalid command-line arguments"
      -- if the file path's extension is ".ir", call runIR
      -- if the file path's extension is ".mlg", call runMalgo
      -- otherwise, print an error message
      logInfo_ $ "Run " <> convertString filePath
      case takeExtension filePath of
        ".ir" -> runIR filePath
        ".mlg" -> runMalgo filePath
        _ -> do
          logAttention_ "Invalid file extension"
          error "Invalid file extension"
  pure ()

runMalgo :: (IOE :> es, Log :> es) => FilePath -> Eff es ()
runMalgo filePath = do
  text <- liftIO $ T.readFile filePath
  defs <- case Surface.parse filePath text of
    Left parseErrorBundle -> do
      logAttention_ $ convertString $ errorBundlePretty parseErrorBundle
      pure []
    Right defs -> do
      logInfo_ $ pShow defs
      pure defs
  for_ defs \def ->
    logInfo_ $ sShow def

runIR :: (UniqueGen :> es, IOE :> es, Log :> es) => FilePath -> Eff es ()
runIR filePath = do
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
