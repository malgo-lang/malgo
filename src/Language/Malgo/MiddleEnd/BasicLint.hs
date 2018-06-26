{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Malgo.MiddleEnd.BasicLint (lintExpr) where

import           Language.Malgo.ID
import           Language.Malgo.IR.IR
import           Language.Malgo.Prelude

lintExpr :: MonadError (Doc ann) m => Expr (ID MType) -> m MType
lintExpr (Let name val body) = do
  val' <- lintExpr val
  if mTypeOf name == val'
    then lintExpr body
    else throwError $ pretty val <+> "cannot assign to:" <+> pretty name <> ":" <> pretty (mTypeOf name)
lintExpr e@(Apply f args) = do
  paramtys <- getParamtys
  if paramtys /= argtys
    then throwError ("expected:" <+> pretty paramtys <> "," <+> "actual:" <+> pretty argtys)
    else return (mTypeOf e)
  where fty = mTypeOf f
        argtys = map mTypeOf args
        getParamtys =
          case fty of
            FunctionTy _ ts -> return ts
            PointerTy (StructTy [FunctionTy _ ts, _]) -> return ts
            t -> throwError $ pretty t <+> "is not applieable"
lintExpr (Access e is) =
  accessMType (mTypeOf e) is
lintExpr (If c t f)
  | mTypeOf c == IntTy 1 = do
      t' <- lintExpr t
      f' <- lintExpr f
      if t' == f'
        then return t'
        else throwError $ pretty t <+> "must be typed as:" <+> pretty f'
  | otherwise = throwError $ pretty c <+> "must be typed as: i1"
lintExpr e = return $ mTypeOf e
