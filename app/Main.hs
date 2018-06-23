{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           Language.Malgo.Driver
import qualified Language.Malgo.Lexer   as Lexer
import qualified Language.Malgo.Parser  as Parser
import           Language.Malgo.Prelude
import           LLVM.Pretty

main :: IO ()
main = do
  opt <- parseOpt
  let file = _srcName opt
  contents <- readFile (toS file)
  let tokens = Lexer.lexing (toS file) (toS contents)
  let parser = Parser.parseExpr
  let ast = case parser <$> tokens of
        Left x  -> error $ show x
        Right x -> x
  ll <- compile file ast opt

  unless (_dumpParsed opt || _dumpRenamed opt
          || _dumpTyped opt || _dumpHIR opt
          || _dumpIR opt
          || _dumpFlatten opt || _dumpClosure opt) $
    putText (toS $ ppllvm ll)
