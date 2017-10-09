module Main where

import           Control.Arrow         ((&&&))
import           Control.Monad.State
import qualified Language.Malgo.Parser as Parser
import qualified Language.Malgo.Syntax as Syntax
import qualified Language.Malgo.Typing as Typing
import           System.Environment    (getArgs)
import qualified Text.Parsec.String    as P
import qualified Text.PrettyPrint      as Pretty

main :: IO ()
main = do
  args <- getArgs
  let file = head args
  result <- P.parseFromFile Parser.parseToplevel file
  case result of
    Left err  -> print err
    Right ast -> do print $ Pretty.sep (map Syntax.prettyDecl ast)
                    print ast
                    let ret = runStateT (mapM Typing.typedDecl ast) Typing.initEnv

                    case ret of
                      Left err -> print err
                      Right (xs, env) -> do
                        print xs
                        print $ map (fst &&& (Syntax.prettyType . snd)) env
