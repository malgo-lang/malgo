{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Data.Text.IO as T
import Language.Griff.Parser (pTopLevel)
import Language.Griff.Rename (rename)
import Language.Griff.RnEnv
import Language.Griff.Typing (typeCheck)
import Language.Malgo.Monad
import Language.Malgo.Prelude
import Language.Malgo.Pretty
import Text.Megaparsec (errorBundlePretty, parse)
import qualified Text.PrettyPrint as P
import qualified Data.Map as Map
import qualified Language.Griff.TcEnv as T
import Language.Griff.Grouping

main :: IO ()
main = do
  src <- T.getContents
  ds <- case parse pTopLevel "<stdin>" src of
    Right ds -> pure ds
    Left err -> error $ errorBundlePretty err
  putStrLn "=== PARSE ==="
  print $ P.sep $ P.punctuate ";" $ map pPrint ds
  void $
    runUniqT ?? UniqSupply 0 $ do
      rnState <- genRnState
      rnEnv <- genRnEnv

      liftIO $ putStrLn "=== RENAME ==="
      ds' <- rename rnState rnEnv ds
      liftIO $ print $ P.sep $ P.punctuate ";" $ map pPrint ds'

      liftIO $ putStrLn "=== CALL GRAPH ==="
      let bindGroup = makeBindGroup ds'
      liftIO $ print $ pPrint $ bindGroup ^. scDefs

      liftIO $ putStrLn "=== TYPE CHECK ==="
      (ds, env) <- typeCheck rnEnv ds'
      liftIO $ print $ pPrint $ Map.toList $ view T.varEnv env
      liftIO $ print $ pPrint $ map (over (_2 . _2) Map.toList) $ Map.toList $ view T.tyConEnv env
      liftIO $ print $ pPrint ds