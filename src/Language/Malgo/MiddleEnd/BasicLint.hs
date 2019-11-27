{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.MiddleEnd.BasicLint (BasicLint, lint, runLint, lintExpr, lintProgram) where

import           Control.Lens
import           Control.Monad.Except  (MonadError, runExcept, throwError)
import           Language.Malgo.ID
import           Language.Malgo.IR.IR
import           Language.Malgo.Pass
import           Language.Malgo.Pretty
import           Relude

data BasicLint

instance Pass BasicLint (Expr (ID MType)) () where
  isDump _ _ = False
  trans _ s =
    case lint s of
      Right _  -> pass
      Left mes -> error $ show mes

lint :: Expr (ID MType) -> Either Doc MType
lint expr = runExcept $ evalStateT (lintExpr expr) []

runLint :: StateT [ID MType] (ExceptT Doc Identity) a -> Either Doc a
runLint = runExcept . flip evalStateT []

defined :: (MonadState [ID MType] m, MonadError Doc m) => ID MType -> m ()
defined a =
  unlessM (elem a <$> get)
  (throwError $ pPrint a <+> "is not defined")

notDefined :: (MonadState [ID MType] m, MonadError Doc m) => ID MType -> m ()
notDefined a =
  unlessM (notElem a <$> get)
  (throwError $ pPrint a <+> "is already defined")

match :: (HasMType a1, HasMType a2, MonadError Doc f, Pretty a1, Pretty a2) => a1 -> a2 -> f ()
match a b = unless (mTypeOf a == mTypeOf b) $
  throwError $ "expected:" <+> pPrint (mTypeOf a)
  $+$ "actual:" <+> pPrint (mTypeOf b)
  $+$ parens (fsep [pPrint a, colon, pPrint b])

lintExpr :: (MonadState [ID MType] m, MonadError Doc m) => Expr (ID MType) -> m MType
lintExpr (Let name val body) = do
  notDefined name
  val' <- lintExpr val
  match name val'
  modify (name:)
  lintExpr body
lintExpr e@(Apply f args) = do
  mapM_ defined (f:args)
  paramtys <- getParamtys
  mapM_ (uncurry match) (zip paramtys argtys)
  return $ mTypeOf e
  where fty = mTypeOf f
        argtys = map mTypeOf args
        getParamtys =
          case fty of
            FunctionTy _ ts -> return ts
            t -> throwError $ pPrint t <+> ("is not applieable: " <> parens (pPrint e))
lintExpr (Access e is) = do
  defined e
  accessMType (mTypeOf e) is
lintExpr (If c t f) = do
  match c $ IntTy 1
  defined c
  t' <- lintExpr t
  f' <- lintExpr f
  match t' f'
  return t'
lintExpr (Var a) = do
  defined a
  return (mTypeOf a)
lintExpr e@(Tuple xs) = do
  mapM_ defined xs
  return (mTypeOf e)
lintExpr (LetRec fundecs body) = do
  modify (map (view _1) fundecs ++)
  mapM_ lintFunDec fundecs
  lintExpr body
lintExpr (Cast ty a) = do
  defined a
  return ty
lintExpr e = return $ mTypeOf e

lintFunDec :: (MonadState [ID MType] m, MonadError Doc m) => (ID MType, [ID MType], Expr (ID MType)) -> m ()
lintFunDec (_, params, fbody) = do
  modify (params <>)
  void (lintExpr fbody)

lintDefn :: (MonadState [ID MType] m, MonadError Doc m) => Defn (ID MType) -> m ()
lintDefn (DefFun _ params fbody) = do
  modify (params ++)
  void (lintExpr fbody)

lintProgram :: (MonadState [ID MType] m, MonadError Doc m) => Program (ID MType) -> m ()
lintProgram (Program _ xs) = do
  modify (map (\(DefFun f _ _) -> f) xs ++)
  mapM_ lintDefn xs
