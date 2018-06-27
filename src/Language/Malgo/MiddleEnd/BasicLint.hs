{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Malgo.MiddleEnd.BasicLint (lint) where

import           Language.Malgo.ID
import           Language.Malgo.IR.IR
import           Language.Malgo.Prelude

lint :: Expr (ID MType) -> Either (Doc ann) MType
lint expr = runExcept $ runReaderT (lintExpr expr) []

defined :: (MonadError (Doc ann) m, MonadReader [ID MType] m) => ID MType -> m ()
defined a =
  ifM (elem a <$> ask)
  (return ())
  (throwError $ pretty a <+> "is not defined")

notDefined :: (MonadError (Doc ann) m, MonadReader [ID MType] m) => ID MType -> m ()
notDefined a =
  ifM (notElem a <$> ask)
  (return ())
  (throwError $ pretty a <+> "is already defined")

lintExpr :: (MonadReader [ID MType] m, MonadError (Doc ann) m) => Expr (ID MType) -> m MType
lintExpr (Let name val body) = do
  notDefined name
  val' <- lintExpr val
  if mTypeOf name == val'
    then local (name:) $ lintExpr body
    else throwError $ pretty val <+> "cannot assign to:" <+> pretty name <> ":" <> pretty (mTypeOf name)
lintExpr e@(Apply f args) = do
  mapM_ defined (f:args)
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
  defined e >> accessMType (mTypeOf e) is
lintExpr (If c t f)
  | mTypeOf c == IntTy 1 = do
      defined c
      t' <- lintExpr t
      f' <- lintExpr f
      if t' == f'
        then return t'
        else throwError $ pretty t <+> "must be typed as:" <+> pretty f'
  | otherwise = throwError $ pretty c <+> "must be typed as: i1"
lintExpr (Var a) =
  defined a >> return (mTypeOf a)
lintExpr e@(Tuple xs) =
  mapM_ defined xs >> return (mTypeOf e)
lintExpr (LetRec fundecs body) =
  local (map (view _1) fundecs ++) $ do
    mapM_ lintFunDec fundecs
    lintExpr body
lintExpr (Cast ty a) =
  defined a >> return ty
lintExpr e = return $ mTypeOf e

lintFunDec :: (MonadError (Doc ann) m, MonadReader [ID MType] m) => (ID MType, Maybe [ID MType], Expr (ID MType)) -> m ()
lintFunDec (_, mparams, fbody) =
  local (fromMaybe [] mparams ++) $ void $ lintExpr fbody
