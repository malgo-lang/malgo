module Main (main) where

import Data.Text.IO qualified as T
import Effectful.Error.Static (prettyCallStack, runError)
import Effectful.Log
import Log.Backend.StandardOutput
import Malgo.Core
import Malgo.Eval (EvalError, eval, newEnv)
import Malgo.Prelude
import Malgo.Unique
import Malgo.Core.Builder

main :: IO ()
main = do
  _ <- withStdOutLogger \stdOutLogger -> do
    runEff $ runUniqueGen $ runLog "compiler" stdOutLogger LogInfo do
      (stmt1, defs1) <- ex1
      stmt1' <- focus stmt1
      defs1' <- traverse focusDefinition defs1
      let env1 = newEnv defs1'
      result <- runError @EvalError $ eval env1 stmt1'
      liftIO $ case result of
        Left (callStack, err) -> do
          putStrLn $ prettyCallStack callStack
          T.putStrLn $ pShow err
        Right _ -> pure ()

  -- (stmt2, defs2) <- runEff $ runUniqueGen $ runLog "compiler" stdOutLogger LogInfo ex2
  -- traverse_ (T.putStrLn . pShow) defs2
  -- T.putStrLn $ pShow stmt2

  pure ()
