module Main where

import           Control.Monad
import           Control.Monad.State.Strict
import           Language.Malgo.Driver
import qualified Language.Malgo.Lexer       as Lexer
import qualified Language.Malgo.Parser      as Parser
import           Language.Malgo.Utils

main :: IO ()
main = do
  opt <- parseOpt
  let file = _srcName opt
  contents <- readFile file
  let tokens = Lexer.lexing file contents
  let ast = case Parser.parseExpr <$> tokens of
        Left x  -> error $ show x
        Right x -> x
  obj <- compile ast opt

  if _compileOnly opt
    then return ()
    else do e <- eval obj
            case e of
              (Left x, _)  ->
                error $ show $ pretty x
              (Right _, _) ->
                return ()
