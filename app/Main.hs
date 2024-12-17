module Main (main) where

import Data.Foldable (traverse_)
import Data.Text.IO qualified as T
import Effectful.Log
import Log.Backend.StandardOutput
import Malgo.Core
import Malgo.Prelude
import Malgo.Unique

main :: IO ()
main = do
  _ <- withStdOutLogger \stdOutLogger -> do
    defs1 <- runEff $ runUniqueGen $ runLog "compiler" stdOutLogger LogInfo ex1
    traverse_ (T.putStrLn . pShow) defs1
    defs2 <- runEff $ runUniqueGen $ runLog "compiler" stdOutLogger LogInfo ex2
    traverse_ (T.putStrLn . pShow) defs2
  pure ()
