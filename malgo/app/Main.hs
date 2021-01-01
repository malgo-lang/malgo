{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Exception (catch, displayException)
import qualified Data.ByteString as BS
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import Koriel.Prelude
import LLVM.Context (withContext)
import LLVM.Module (moduleLLVMAssembly, withModuleFromAST)
import LLVM.Pretty (ppllvm)
import Language.Malgo.Driver
import Language.Malgo.Monad (Opt (dstName, srcName, viaBinding))
import System.Exit (exitFailure)
import System.IO (hPutStr, stderr)

main :: IO ()
main =
  do
    opt <- parseOpt
    src <- T.readFile (srcName opt)
    ll <- compile opt src
    if viaBinding opt
      then withContext $ \ctx ->
        BS.writeFile (dstName opt) =<< withModuleFromAST ctx ll moduleLLVMAssembly
      else TL.writeFile (dstName opt) (ppllvm ll)
    `catch` \(e :: Bug) -> do
      -- bugで投げられた例外をcatchする
      hPutStr stderr (displayException e)
      exitFailure
