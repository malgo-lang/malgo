{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import LLVM.Pretty (ppllvm)
import Language.Malgo.Driver
import Language.Malgo.Monad
import Language.Malgo.Prelude

main :: IO ()
main = do
  opt <- parseOpt
  src <- T.readFile (srcName opt)

  if isInterpretMode opt
    then do
      ret <- interpret opt src
      print ret
    else do
      ll <- compile opt src
      let (x : xs) = TL.lines $ ppllvm ll
      let ir = TL.unlines (x : ("source_filename = " <> TL.pack (show (srcName opt))) : xs)
      TL.writeFile (dstName opt) ir