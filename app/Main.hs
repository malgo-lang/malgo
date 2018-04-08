module Main where

import           Language.Malgo.Driver
import qualified Language.Malgo.Lexer     as Lexer
import qualified Language.Malgo.Parser    as Parser
import qualified Language.Malgo.Prelude   as P
import           LLVM.Pretty

main :: IO ()
main = do
  opt <- parseOpt
  let file = _srcName opt
  contents <- readFile (P.toS file)
  let tokens = Lexer.lexing (P.toS file) contents
  let parser = Parser.parseExpr
  let ast = case parser <$> tokens of
        Left x  -> error $ show x
        Right x -> x
  ll <- compile file ast opt

  P.unless (_dumpParsed opt || _dumpRenamed opt
            || _dumpTyped opt || _dumpHIR opt
            || _dumpFlatten opt || _dumpClosure opt) $
    P.putText (P.toS $ ppllvm ll)
