{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Exception (catch, displayException)
import qualified Data.ByteString as BS
import qualified Data.Text.IO as T
import LLVM.Context
import LLVM.Module
import Language.Malgo.Driver
import Language.Malgo.Monad
import Language.Malgo.Prelude
import System.Exit (exitFailure)
import System.IO (hPutStr, stderr)

-- import LLVM.Pretty (ppllvm)
-- import qualified Data.Text.Lazy.IO as TL

main :: IO ()
main =
  ( do
      opt <- parseOpt
      src <- T.readFile (srcName opt)
      if isInterpretMode opt
        then void $ interpret opt src
        else do
          ll <- (if isCoreMode opt then compileCore else compile) opt src
          -- TL.writeFile (dstName opt) (ppllvm ll)
          withContext $ \ctx -> do
            llvm <- withModuleFromAST ctx ll moduleLLVMAssembly
            BS.writeFile (dstName opt) llvm
  )
    `catch` \e -> do
      hPutStr stderr (displayException (e :: Bug))
      exitFailure
