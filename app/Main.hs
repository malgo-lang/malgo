{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import           Language.Malgo.Driver
import qualified Language.Malgo.Lexer  as Lexer
import           Language.Malgo.Monad
import qualified Language.Malgo.Parser as Parser
import           LLVM.AST              (Module)
import           LLVM.Context          (withContext)
import           LLVM.Module           (moduleLLVMAssembly, withModuleFromAST)
import           Relude

main :: IO ()
main = do
  opt <- parseOpt

  let file = srcName opt
  tokens <- Lexer.tokenize () file =<< readFile file
  let parser = Parser.parseExpr
  let ast = case parser <$> tokens of
              Left x  -> error $ show x
              Right x -> x

  ll <- compile file ast opt

  writeFileText (dstName opt) =<< ppllvm ll

ppllvm :: LLVM.AST.Module -> IO Text
ppllvm ast = withContext $ \ctx ->
  decodeUtf8 @Text <$> withModuleFromAST ctx ast moduleLLVMAssembly
