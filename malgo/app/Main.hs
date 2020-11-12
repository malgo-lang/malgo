{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Exception (catch, displayException)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import Koriel.Prelude
import LLVM.Pretty (ppllvm)
import Language.Malgo.Driver
import Language.Malgo.Monad
import System.Exit (exitFailure)
import System.IO (hPutStr, stderr)
import qualified Data.ByteString as BS
import LLVM.Context (withContext)
import LLVM.Module (moduleLLVMAssembly, withModuleFromAST)

main :: IO ()
main =
  ( do
      opt <- parseOpt
      src <- T.readFile (srcName opt)
      ll <- compile opt src
      if viaBinding opt
        then withContext $ \ctx ->
          BS.writeFile (dstName opt) =<< withModuleFromAST ctx ll moduleLLVMAssembly
        else TL.writeFile (dstName opt) (ppllvm ll)
  )
    `catch` \e -> do
      hPutStr stderr (displayException (e :: Bug))
      exitFailure
