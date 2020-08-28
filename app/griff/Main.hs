{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as T
import Language.Griff.Parser (pTopLevel)
import Text.Megaparsec (errorBundlePretty, parse)
import qualified Text.PrettyPrint as P
import Language.Malgo.Pretty

main :: IO ()
main = do
  src <- T.getContents
  ds <- case parse pTopLevel "<stdin>" src of
    Right ds -> pure ds
    Left err -> error $ errorBundlePretty err
  print $ P.sep $ P.punctuate ";" $ map pPrint ds