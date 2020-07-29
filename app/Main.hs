{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Exception (catch, displayException)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import LLVM.Pretty (ppllvm)
import Language.Malgo.Driver
import Language.Malgo.Monad
import Language.Malgo.Prelude
import System.Exit (exitFailure)
import System.IO (hPutStr, stderr)

main :: IO ()
main =
  ( do
      opt <- parseOpt
      src <- T.readFile (srcName opt)
      ll <- compile opt src
      TL.writeFile (dstName opt) (ppllvm ll)
  )
    `catch` \e -> do
      hPutStr stderr (displayException (e :: Bug))
      exitFailure
