{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           Language.Malgo.Driver
import qualified Language.Malgo.Lexer  as Lexer
import           Language.Malgo.Monad
import qualified Language.Malgo.Parser as Parser
import           LLVM.Pretty
import           Universum

main :: IO ()
main = do
  opt <- parseOpt
  let file = srcName opt
  contents <- readFile (toString file)
  let tokens = runIdentity $ Lexer.lexing () (toString file) contents
  let parser = Parser.parseExpr
  let ast = case parser <$> tokens of
        Left x  -> error $ show x
        Right x -> x

  u <- newIORef 0
  ll <- compile file ast (UniqSupply u) opt

  unless (dumpParsed opt
          || dumpRenamed opt
          || dumpTyped opt
          || dumpKNormal opt
          || dumpTypeTable opt
          || dumpClosure opt) $
    putStrLn $ ppllvm ll
