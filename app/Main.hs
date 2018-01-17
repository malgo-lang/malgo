{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import           Control.Monad
import           Control.Monad.State
import qualified Language.Malgo.Beta      as Beta
import qualified Language.Malgo.Closure   as Closure
import qualified Language.Malgo.Eval      as Eval
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

compile ::
  Monad m =>
  Syntax.Expr Name
  -> StateT Int m (MIR.Program TypeCheck.TypedID)
compile ast = do
  renamed <- doMalgoT (Rename.rename ast)
    >>= \case Left x  -> error $ show x
              Right x -> return x
  typechecked <- doMalgoT (TypeCheck.typeCheck renamed)
    >>= \case Left x -> error $ show x
              Right x -> return x
  knormal <- doMalgoT (KNormal.knormal typechecked)
    >>= \case Left x -> error $ show x
              Right x -> return x
  beta <- doMalgoT (Beta.betaTrans knormal)
    >>= \case Left x -> error $ show x
              Right x -> return x
  mir <- doMalgoT (Closure.conv (Flatten.flatten beta))
    >>= \case Left x -> error $ show x
              Right x -> return x
  return mir

eval ::
  Syntax.Expr Name
  -> IO (Either MalgoError Eval.Value)
eval ast = flip evalStateT 0 $ do
  mir <- compile ast
  doMalgoT (Eval.eval mir)

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

  e <- eval ast

  let result = case e of
        Right x -> x
        Left x  -> error $ show $ pretty x

  seq result $ return ()
