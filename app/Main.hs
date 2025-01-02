module Main (main) where

import Data.Text.IO qualified as T
import Effectful.Error.Static (prettyCallStack, runError)
import Effectful.Log
import Log.Backend.StandardOutput
import Malgo.Core
import Malgo.Core.Builder
import Malgo.Eval (EvalError, eval, newEnv)
import Malgo.Prelude
import Malgo.Unique

main :: IO ()
main = do
  _ <- withStdOutLogger \stdOutLogger -> do
    runEff $ runUniqueGen $ runLog "compiler" stdOutLogger LogInfo do
      logInfo_ "Run ex1"
      runExample ex1
      logInfo_ "Run ex2"
      runExample ex2

  pure ()

runExample :: (UniqueGen :> es, Log :> es,  IOE :> es) => Eff es (Statement, [Definition]) -> Eff es ()
runExample example = do
  (stmt, defs) <- example
  stmt' <- focus stmt
  defs' <- traverse focusDefinition defs
  let env = newEnv defs'
  result <- runError @EvalError $ eval env stmt'
  liftIO $ case result of
    Left (callStack, err) -> do
      putStrLn $ prettyCallStack callStack
      T.putStrLn $ pShow err
    Right _ -> pure ()