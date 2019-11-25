module Main where

import qualified Data.Text.Lazy.IO     as T
import           Language.Malgo.Driver
import qualified Language.Malgo.Lexer  as Lexer
import           Language.Malgo.Monad
import qualified Language.Malgo.Parser as Parser
import           LLVM.Pretty
import           Data.IORef

main :: IO ()
main = do
  opt <- parseOpt

  let file = srcName opt
  tokens <- Lexer.lexing () file =<< readFile file
  let parser = Parser.parseExpr
  let ast = case parser <$> tokens of
              Left x  -> error $ show x
              Right x -> x

  u <- newIORef 0
  ll <- compile file ast (UniqSupply u) opt

  T.writeFile (dstName opt) (ppllvm ll)
