{-# LANGUAGE NoImplicitPrelude #-}
module Language.Malgo.FrontEnd.Driver where

import           Language.Malgo.FrontEnd.Lexer
import           Language.Malgo.FrontEnd.Parser
import           Language.Malgo.FrontEnd.Rename
import           Language.Malgo.FrontEnd.RnTcEnv
import           Language.Malgo.FrontEnd.TypeCheck
import           Language.Malgo.IR.AST
import           Universum                         hiding (Type)

frontend filename = do
  source <- readFile filename
  Right toks <- lex filename source
  let decs = parse toks
  rnTcEnv <- makeRnTcEnv
  usingStateT rnTcEnv $ do
    decs' <- rename decs
    typeCheck decs'
    return decs'
