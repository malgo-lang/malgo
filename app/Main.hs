{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           Language.Malgo.Driver
import           Language.Malgo.Monad
import           LLVM.Pretty (ppllvm)
import           Relude

main :: IO ()
main = do
  opt <- parseOpt

  ll  <- compile opt =<< readFileText (srcName opt)

  writeFileLText (dstName opt) $ ppllvm ll