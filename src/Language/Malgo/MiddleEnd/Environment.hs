{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Malgo.MiddleEnd.Environment where

import           Language.Malgo.ID
import           Language.Malgo.IR.IR
import           Language.Malgo.Monad
import           Language.Malgo.Prelude

-- | predefined functions
prelude :: MonadMalgo s m => m [Defn (ID MType)]
prelude = do
  addInt <- newID "add_int" (FunctionTy (IntTy 32) [IntTy 32, IntTy 32])
  mapM exToFun [ DefEx addInt "add_int" ]
