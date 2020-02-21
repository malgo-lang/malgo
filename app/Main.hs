{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import           Language.Malgo.Driver
import           Language.Malgo.Monad
import           LLVM.AST                       ( Module )
import           LLVM.Context                   ( withContext )
import           LLVM.Module                    ( moduleLLVMAssembly
                                                , withModuleFromAST
                                                )
import           Relude

import qualified Data.Text.IO                  as T

main :: IO ()
main = do
  opt <- parseOpt

  ll  <- compile opt =<< T.readFile (srcName opt)

  writeFileText (dstName opt) =<< ppllvm ll

ppllvm :: LLVM.AST.Module -> IO Text
ppllvm ast =
  withContext $ \ctx -> decodeUtf8 @Text <$> withModuleFromAST ctx ast moduleLLVMAssembly
