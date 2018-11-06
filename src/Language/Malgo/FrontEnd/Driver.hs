{-# LANGUAGE NoImplicitPrelude #-}
module Language.Malgo.FrontEnd.Driver where

import           Language.Malgo.FrontEnd.Rename
import           Language.Malgo.FrontEnd.RnTcEnv
import           Language.Malgo.FrontEnd.TypeCheck
import           Language.Malgo.IR.AST
import           Universum                         hiding (Type)

frontend decs = do
  rnTcEnv <- makeRnTcEnv
  usingStateT rnTcEnv $ do
    decs' <- rename decs
    typeCheck decs'
    return decs'
