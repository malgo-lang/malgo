{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           Data.Outputable
import qualified Data.Text.Lazy.IO                 as T
import qualified Data.Map as Map
import           Language.Malgo.Old.Driver
import qualified Language.Malgo.Old.Lexer          as Lexer
import           Language.Malgo.Old.Monad
import qualified Language.Malgo.Old.Parser         as Parser
import           Language.Malgo.Pretty
import           LLVM.Pretty
import           Universum

main :: IO ()
main = do
  opt <- parseOpt

  let file = srcName opt
  tokens <- Lexer.lexing () (toString file) =<< readFile file
  let parser = Parser.parseExpr
  let ast = case parser <$> tokens of
              Left x  -> error $ show x
              Right x -> x

  u <- newIORef 0
  ll <- compile file ast (UniqSupply u) opt

  T.writeFile (dstName opt) (ppllvm ll)
