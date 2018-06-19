{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Malgo.MiddleEnd.Environment where

import           Language.Malgo.IR.IR

-- | predefined functions
prelude :: [Expr a]
prelude =
  [ Prim "add_int" (FunctionTy (IntTy 32) [IntTy 32, IntTy 32])
  , Prim "add_float" (FunctionTy DoubleTy [DoubleTy, DoubleTy])
  ]
