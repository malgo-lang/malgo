{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Exception (catch, displayException)
import Language.Griff.Driver (compile)
import Language.Griff.Prelude
import System.Exit (exitFailure)
import System.IO (hPutStr, stderr)

main :: IO ()
main =
  (compile =<< parseOpt) `catch` \e -> do
    hPutStr stderr (displayException (e :: Bug))
    exitFailure
