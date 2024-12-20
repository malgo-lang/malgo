module Main (main) where

import Data.Foldable (traverse_)
import Data.Text.IO qualified as T
import Effectful.Error.Static (runError, prettyCallStack)
import Effectful.Log
import Log.Backend.StandardOutput
import Malgo.Core
import Malgo.Eval (EvalError, eval, newEnv)
import Malgo.Prelude
import Malgo.Unique

main :: IO ()
main = do
  _ <- withStdOutLogger \stdOutLogger -> do
    (stmt1, defs1) <- runEff $ runUniqueGen $ runLog "compiler" stdOutLogger LogInfo ex1
    traverse_ (T.putStrLn . pShow) defs1
    T.putStrLn $ pShow stmt1
    let env1 = newEnv defs1
    result <- runEff $ runLog "eval" stdOutLogger LogInfo $ runError @EvalError $ eval env1 stmt1
    case result of
      Left (callStack, err) -> do
        putStrLn $ prettyCallStack callStack
        T.putStrLn $ pShow err
      Right _ -> pure ()

  -- (stmt2, defs2) <- runEff $ runUniqueGen $ runLog "compiler" stdOutLogger LogInfo ex2
  -- traverse_ (T.putStrLn . pShow) defs2
  -- T.putStrLn $ pShow stmt2

  pure ()
