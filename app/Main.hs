{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import qualified Language.Malgo.Beta      as Beta
import qualified Language.Malgo.Flatten   as Flatten
import qualified Language.Malgo.KNormal   as KNormal
import qualified Language.Malgo.Lexer     as Lexer
import qualified Language.Malgo.Parser    as Parser
import qualified Language.Malgo.Rename    as Rename
import qualified Language.Malgo.TypeCheck as TypeCheck
import qualified Language.Malgo.Closure as Closure
import           Language.Malgo.Utils

import           System.Environment       (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let file = head args
  contents <- readFile file
  let tokens = Lexer.lexing file contents

  let ast' = Parser.parseExpr <$> tokens

  let ast = case ast' of
        Left x  -> error $ show x
        Right x -> x

  let (renamedAST, rnenv) = case Rename.rename ast of
        (Right x, e) -> (x, e)
        (Left x, _)  -> error $ show $ pretty x

  let typedAST = case TypeCheck.typeCheck renamedAST of
        (Right x, _) -> x
        (Left x, _)  -> error $ show $ pretty x

  let (knormal, kenv) = case KNormal.knormal rnenv typedAST of
        (Right x, e) -> (x, e)
        (Left x, _)  -> error $ show $ pretty x

  let beta = case Beta.betaTrans knormal of
        (Right x, _) -> x
        (Left x, _)  -> error $ show $ pretty x

  let flatten = Flatten.flatten beta

  let closure = case Closure.conv (KNormal._count kenv) flatten of
        (Right x, _) -> x
        (Left x, _) -> error $ show $ pretty x

  print $ pretty closure
