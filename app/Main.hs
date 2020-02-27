{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Language.Malgo.Driver
import           Language.Malgo.Monad
import           LLVM.Pretty (ppllvm)
import           Relude

main :: IO ()
main = do
  opt <- parseOpt

  ll  <- compile opt =<< readFileText (srcName opt)

  let (x:xs) = lines $ toText $ ppllvm ll
  let ir = unlines (x : ("source_filename = " <> show (srcName opt)) : xs)

  writeFileText (dstName opt) ir