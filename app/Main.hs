{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

-- import qualified Language.Malgo.Beta      as Beta
-- import qualified Language.Malgo.Closure   as Closure
-- import qualified Language.Malgo.Codegen   as Codegen
-- import qualified Language.Malgo.IRBuilder as IRBuilder
import qualified Language.Malgo.Flatten   as Flatten
import qualified Language.Malgo.KNormal   as KNormal
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
  -- print tokens

  let ast' = Parser.parseExpr <$> tokens

  let ast = case ast' of
        Left x  -> error $ show x
        Right x -> x
  -- print ast
  print $ pretty ast

  let (renamedAST, rnenv) = case Rename.rename ast of
        (Right x, e) -> (x, e)
        (Left x, _)  -> error $ show x

  print $ pretty renamedAST

  let typedAST = case TypeCheck.typeCheck renamedAST of
        (Right x, _) -> x
        (Left x, _)  -> error $ show x

  print $ pretty typedAST

  let knormal = case KNormal.knormal rnenv typedAST of
        (Right x, _) -> x
        (Left x, _)  -> error $ show x

  print $ pretty knormal

  let flatten = Flatten.flattenProgram knormal
  print $ pretty flatten
  -- let beta = join $ Beta.betaTrans <$> fmap fst kNormal
  -- print $ pretty beta

  -- let assoc = Assoc.assoc <$> beta
  -- print $ pretty assoc

  -- let Right (mir, count) = case join $ MIR.toMIR <$> assoc <*> fmap snd kNormal of
  --       Right x -> Right x
  --       Left x  -> error x
  -- print $ pretty mir

  -- let Right (cls, env) = case Closure.runClosure count $ Closure.trans mir of
  --       Right x -> Right x
  --       Left x  -> error x

  -- print $ pretty cls
  -- print $ PP.sep $ map pretty (Closure.toplevel env)

  -- let llvm = Codegen.trans file (Closure.toplevel env) cls
  -- -- print llvm

  -- case llvm of
  --   Right x -> do
  --     ir <- Codegen.emit x
  --     BS.putStrLn ir
  --   Left x -> print x
