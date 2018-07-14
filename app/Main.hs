{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           Language.Malgo.Driver
import qualified Language.Malgo.Lexer  as Lexer
import           Language.Malgo.Monad
import qualified Language.Malgo.Parser as Parser
import           LLVM.Pretty
import           RIO
import qualified RIO.Text              as Text
import qualified RIO.Text.Lazy         as TL
import           System.IO

main :: IO ()
main = do
  opt <- parseOpt
  let file = _srcName opt
  contents <- readFile (Text.unpack file)
  let tokens = Lexer.lexing (Text.unpack file) contents
  let parser = Parser.parseExpr
  let ast = case parser <$> tokens of
        Left x  -> error $ show x
        Right x -> x

  u <- newIORef 0
  ll <- compile file ast opt (UniqSupply u)

  unless (_dumpParsed opt
          || _dumpRenamed opt
          || _dumpTyped opt
          || _dumpKNormal opt
          || _dumpClosure opt) $
    putStrLn $ TL.unpack $ ppllvm ll
