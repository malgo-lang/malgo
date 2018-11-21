{-# LANGUAGE NoImplicitPrelude #-}
module Language.Malgo.FrontEnd.Driver where

import           Language.Malgo.FrontEnd.Rename
import           Language.Malgo.FrontEnd.RnTcEnv
import           Language.Malgo.FrontEnd.TypeCheck
import           Language.Malgo.Id
import           Language.Malgo.IR.AST
import           Language.Malgo.Monad
import           Universum               hiding ( Type )

frontend :: MonadMalgo m => Program Text -> m (Program Id, RnTcEnv)
frontend program = do
  rnTcEnv <- makeRnTcEnv
  usingStateT rnTcEnv $ do
    program' <- rename program
    typeCheck program'
    return program'
