module Main where

import qualified Language.Malgo.Beta    as Beta
import qualified Language.Malgo.KNormal as KNormal
import qualified Language.Malgo.Parser  as Parser
import qualified Language.Malgo.Typing  as Typing
import           System.Environment     (getArgs)
import qualified Text.Parsec.String     as P

joinE :: (Show e, Show e') => Either e (Either e' a) -> Either String a
joinE (Left x)          = Left (show x)
joinE (Right (Left x))  = Left (show x)
joinE (Right (Right x)) = Right x

main :: IO ()
main = do
  args <- getArgs
  let file = head args
  ast <- P.parseFromFile Parser.parseToplevel file
  print ast

  let typedAST = joinE $ Typing.typing (mapM (mapM Typing.typeofDecl) ast)
  print typedAST

  let kNormal = joinE $ KNormal.knormal (mapM (mapM KNormal.transDecl) typedAST)
  print kNormal

  let beta = joinE $ Beta.betaTrans (mapM (mapM Beta.transDecl) kNormal)
  print beta
