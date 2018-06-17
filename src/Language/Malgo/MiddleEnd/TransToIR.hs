{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.Malgo.MiddleEnd.TransToIR where

import           Language.Malgo.ID
import           Language.Malgo.IR.IR
import qualified Language.Malgo.IR.Syntax             as S
import           Language.Malgo.MiddleEnd.Environment
import           Language.Malgo.Monad
import           Language.Malgo.Prelude
import           Language.Malgo.TypedID

data TEnv = TEnv { _uniqSupply :: UniqSupply }

makeLenses ''TEnv

instance MalgoEnv TEnv where
  uniqSupplyL = uniqSupply
  genEnv = TEnv

throw :: MonadMalgo TEnv m => Info -> Doc ann -> m a
throw info mes = malgoError $ "error(transToIR):" <+> pretty info <+> mes

trans :: MonadMalgo TEnv m => S.Expr TypedID -> m (Expr (ID MType))
trans e = transToIR =<< transType e

transType :: MonadMalgo TEnv m => S.Expr TypedID -> m (S.Expr (ID MType))
transType = undefined

transToIR :: MonadMalgo TEnv m => S.Expr (ID MType) -> m (Expr (ID MType))
transToIR = undefined
