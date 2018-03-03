module Main where

import           Language.Malgo.Driver
import qualified Language.Malgo.Lexer     as Lexer
import qualified Language.Malgo.Parser    as Parser
import qualified Language.Malgo.Prelude   as P
import qualified Language.Malgo.RevParser as RevParser
import           LLVM.Pretty

main :: IO ()
main = do
  opt <- parseOpt
  let file = _srcName opt
  contents <- readFile (P.toS file)
  let tokens = Lexer.lexing (P.toS file) contents
  let parser = if _useRevisedSyntax opt
               then RevParser.parseExpr
               else Parser.parseExpr
  let ast = case parser <$> tokens of
        Left x  -> error $ show x
        Right x -> x
  ll <- compile file ast opt

  P.putText (P.toS $ ppllvm ll)
