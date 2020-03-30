{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Language.Malgo.Driver
import           Language.Malgo.Monad
import           Language.Malgo.Prelude
import           LLVM.Pretty                    ( ppllvm )
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.IO             as TL

main :: IO ()
main = do
  opt <- parseOpt

  ll  <- compile opt =<< T.readFile (srcName opt)

  let (x : xs) = TL.lines $ ppllvm ll
  let ir = TL.unlines (x : ("source_filename = " <> TL.pack (show (srcName opt))) : xs)

  TL.writeFile (dstName opt) ir
