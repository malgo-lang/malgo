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
  let file = _srcName opt
  contents <- readFile (toString file)
  let tokens = Lexer.lexing (toString file) (toString contents)
  let parser = Parser.parseExpr
  let ast = case parser <$> tokens of
        Left x  -> error $ show x
        Right x -> x

  u <- newIORef 0
  ll <- compile file ast (UniqSupply u) opt

  unless (_dumpParsed opt
          || _dumpRenamed opt
          || _dumpTyped opt
          || _dumpKNormal opt
          || _dumpTypeTable opt
          || _dumpClosure opt) $
    putStrLn $ ppllvm ll
