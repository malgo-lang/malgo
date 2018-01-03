{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

-- import qualified Language.Malgo.Beta      as Beta
-- import qualified Language.Malgo.Closure   as Closure
-- import qualified Language.Malgo.Codegen   as Codegen
-- import qualified Language.Malgo.IRBuilder as IRBuilder
-- import qualified Language.Malgo.Beta          as Beta
import qualified Language.Malgo.Flatten   as Flatten
-- import qualified Language.Malgo.FreeVars  as FV
-- import qualified Language.Malgo.HIR       as HIR
import qualified Language.Malgo.KNormal   as KNormal
-- import qualified Language.Malgo.LambdaLifting as LL
import qualified Language.Malgo.Lexer     as Lexer
import qualified Language.Malgo.Parser    as Parser
import qualified Language.Malgo.Rename    as Rename
import qualified Language.Malgo.TypeCheck as TypeCheck
import           Language.Malgo.Utils

import           Control.Monad            (join)
import qualified Data.ByteString.Char8    as BS
import           Data.String
import           System.Environment       (getArgs)
import qualified Text.Parsec.String       as P
import qualified Text.PrettyPrint         as PP

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
  -- print $ pretty ast

  let (renamedAST, rnenv) = case Rename.rename ast of
        (Right x, e) -> (x, e)
        (Left x, _)  -> error $ show $ pretty x

  -- print $ pretty renamedAST

  let typedAST = case TypeCheck.typeCheck renamedAST of
        (Right x, _) -> x
        (Left x, _)  -> error $ show $ pretty x

  -- print $ pretty typedAST

  let knormal = case KNormal.knormal rnenv typedAST of
        (Right x, _) -> x
        (Left x, _)  -> error $ show $ pretty x

  -- print $ pretty knormal

  let flatten = Flatten.flattenProgram knormal
  print $ pretty flatten
