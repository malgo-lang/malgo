module Main where

import qualified Language.Malgo.Parser as Parser
import qualified Language.Malgo.Syntax as Syntax
import           System.Environment    (getArgs)
import qualified Text.Parsec.String    as P

main :: IO ()
main = do
  args <- getArgs
  let file = head args
  result <- P.parseFromFile Parser.parseToplevel file
  case result of
    Left err  -> print err
    Right ast -> print ast
