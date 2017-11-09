{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import           Control.Monad         (join)
-- import qualified Language.Malgo.Beta        as Beta
-- import qualified Language.Malgo.KNormal     as KNormal
import qualified Language.Malgo.Lexer  as Lexer
import qualified Language.Malgo.Parser as Parser
import           Language.Malgo.Utils
-- import           Language.Malgo.PrettyPrint
import qualified Language.Malgo.Typing as Typing
import           System.Environment    (getArgs)
import qualified Text.Parsec.String    as P
import qualified Text.PrettyPrint      as PP

main :: IO ()
main = do
  args <- getArgs
  let file = head args
  contents <- readFile file
  let ast' = Parser.parse <$> Lexer.lexing file contents

  let ast = case ast' of
        Left x  -> Left $ show x
        Right x -> Right x
  print ast
  print $ pretty ast

  let typedAST = join $ Typing.typing (mapM Typing.typeofExpr ast)

  print typedAST
  print $ pretty typedAST

--   let kNormal = join $ KNormal.knormal (mapM (mapM KNormal.transDecl) typedAST)
--   print $ PP.vcat . map pretty <$> kNormal

--   let beta = join $ Beta.betaTrans (mapM (mapM Beta.transDecl) kNormal)
--   print $ PP.vcat . map pretty <$> beta
