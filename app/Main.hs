{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import           Control.Monad          (join)
import qualified Language.Malgo.Assoc   as Assoc
import qualified Language.Malgo.Beta    as Beta
-- import qualified Language.Malgo.Closure as Closure
import qualified Language.Malgo.KNormal as KNormal
import qualified Language.Malgo.Lexer   as Lexer
import qualified Language.Malgo.MIR     as MIR
import qualified Language.Malgo.Parser  as Parser
import qualified Language.Malgo.Typing  as Typing
import           Language.Malgo.Utils
import           System.Environment     (getArgs)
import qualified Text.Parsec.String     as P
import qualified Text.PrettyPrint       as PP

main :: IO ()
main = do
  args <- getArgs
  let file = head args
  contents <- readFile file
  let tokens = Lexer.lexing file contents
  print tokens

  let ast' = Parser.parse <$> tokens

  let ast = case ast' of
        Left x  -> Left $ show x
        Right x -> Right x
  print ast
  print $ pretty ast

  let typedAST = join $ Typing.typing <$> ast

  print typedAST
  print $ pretty typedAST

  let kNormal = join $ KNormal.knormal <$> typedAST
  print $ pretty (fmap fst kNormal)

  let beta = join $ Beta.betaTrans <$> fmap fst kNormal
  print $ pretty beta

  let assoc = Assoc.assoc <$> beta
  print $ pretty assoc

  let Right (mir, count) = join $ MIR.toMIR <$> assoc <*> fmap snd kNormal
  print $ pretty mir

  -- let Right (cls, env) = Closure.runClosure $ mapM Closure.freeVarsInstr (MIR.blockBody mir)
  -- print cls
