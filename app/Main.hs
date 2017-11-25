{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import qualified Language.Malgo.Assoc     as Assoc
import qualified Language.Malgo.Beta      as Beta
import qualified Language.Malgo.Closure   as Closure
import qualified Language.Malgo.Codegen   as Codegen
import qualified Language.Malgo.IRBuilder as IRBuilder
import qualified Language.Malgo.KNormal   as KNormal
import qualified Language.Malgo.Lexer     as Lexer
import qualified Language.Malgo.MIR       as MIR
import qualified Language.Malgo.Parser    as Parser
import qualified Language.Malgo.Typing    as Typing
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

  let ast' = Parser.parse <$> tokens

  let ast = case ast' of
        Left x  -> Left $ show x
        Right x -> Right x
  -- print ast
  -- print $ pretty ast

  let typedAST = join $ Typing.typing <$> ast

  -- print typedAST
  -- print $ pretty typedAST

  let kNormal = join $ KNormal.knormal <$> typedAST
  -- print $ pretty (fmap fst kNormal)

  let beta = join $ Beta.betaTrans <$> fmap fst kNormal
  -- print $ pretty beta

  let assoc = Assoc.assoc <$> beta
  -- print $ pretty assoc

  let Right (mir, count) = join $ MIR.toMIR <$> assoc <*> fmap snd kNormal
  -- print $ pretty mir

  let Right (cls, env) = Closure.runClosure count $ Closure.trans mir
  -- print $ pretty cls
  -- print $ PP.sep $ map pretty (Closure.toplevel env)

  let llvm = Codegen.trans file (Closure.toplevel env) cls
  -- print llvm

  case llvm of
    Right x -> do
      ir <- Codegen.emit x
      BS.putStrLn ir
    Left x -> print x
