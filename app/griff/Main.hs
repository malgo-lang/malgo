{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Exception (catch, displayException)
import Language.Griff.Driver (compile)
import Language.Griff.Option (parseOpt)
import Language.Malgo.Prelude
import System.Exit (exitFailure)
import System.IO (hPutStr, stderr)

main :: IO ()
main =
  (compile =<< parseOpt) `catch` \e -> do
    hPutStr stderr (displayException (e :: Bug))
    exitFailure