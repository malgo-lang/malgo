{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           Control.Monad.State
import qualified Language.Malgo.Beta      as Beta
import qualified Language.Malgo.Closure   as Closure
import           Language.Malgo.Driver
import qualified Language.Malgo.Eval      as Eval
import qualified Language.Malgo.Flatten   as Flatten
import qualified Language.Malgo.KNormal   as KNormal
import qualified Language.Malgo.Lexer     as Lexer
import qualified Language.Malgo.MIR       as MIR
import qualified Language.Malgo.Parser    as Parser
import qualified Language.Malgo.Rename    as Rename
import qualified Language.Malgo.Syntax    as Syntax
import qualified Language.Malgo.TypeCheck as TypeCheck
import           Language.Malgo.Utils
import           System.Environment       (getArgs)

main :: IO ()
main = do
  opt <- parseOpt
  let file = _srcName opt
  contents <- readFile file
  let tokens = Lexer.lexing file contents
  let ast = case Parser.parseExpr <$> tokens of
        Left x  -> error $ show x
        Right x -> x
  let obj = compile ast opt
  e <- eval obj
  let result = case e of
        Left x  -> error $ show $ pretty x
        Right x -> x
  seq result $ return ()
  -- args <- getArgs
  -- let file = head args
  -- contents <- readFile file
  -- let tokens = Lexer.lexing file contents
  -- let ast' = Parser.parseExpr <$> tokens
  -- let ast =
  --       case ast' of
  --         Left x  -> error $ show x
  --         Right x -> x
  -- e <- eval ast
  -- let result =
  --       case e of
  --         Right x -> x
  --         Left x  -> error $ show $ pretty x
  -- seq result $ return ()
