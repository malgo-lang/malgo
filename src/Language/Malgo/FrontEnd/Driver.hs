{-# LANGUAGE NoImplicitPrelude #-}
module Language.Malgo.FrontEnd.Driver where

import           Language.Malgo.FrontEnd.Rename
import           Language.Malgo.FrontEnd.RnTcEnv
import           Language.Malgo.FrontEnd.TypeCheck
import           Language.Malgo.Id
import           Language.Malgo.IR.AST
import           Language.Malgo.Monad
import           Universum               hiding ( Type )

frontend :: MonadMalgo m => [Decl Text] -> m ([Decl Id], RnTcEnv)
frontend decs = do
  rnTcEnv <- makeRnTcEnv
  usingStateT rnTcEnv $ do
    decs' <- rename decs
    typeCheck decs'
    return decs'
