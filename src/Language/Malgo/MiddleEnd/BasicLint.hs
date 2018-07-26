{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Malgo.MiddleEnd.BasicLint (lint, runLint, lintExpr, lintProgram) where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Language.Malgo.IR.IR
import           Language.Malgo.Pretty
import           Lens.Micro.Platform   (_1)
import           RIO

lint :: (HasMType a, Eq a, Pretty a) => Expr a -> Either Doc MType
lint expr = runExcept $ evalStateT (lintExpr expr) []

runLint :: StateT [a1] (ExceptT e Identity) a2 -> Either e a2
runLint = runExcept . flip evalStateT []

ifM :: Monad m => m Bool -> m b -> m b -> m b
ifM b t f = do b' <- b; if b' then t else f

defined :: (Foldable t, Eq a, MonadState (t a) m, MonadError Doc m, Pretty a) => a -> m ()
defined a =
  ifM (elem a <$> get)
  (return ())
  (throwError $ pPrint a <+> "is not defined")

notDefined :: (Foldable t, Eq a, MonadState (t a) m, MonadError Doc m, Pretty a) => a -> m ()
notDefined a =
  ifM (notElem a <$> get)
  (return ())
  (throwError $ pPrint a <+> "is already defined")

lintExpr :: (MonadState [a] m, Eq a, HasMType a, MonadError Doc m, Pretty a) => Expr a -> m MType
lintExpr (Let name val body) = do
  notDefined name
  val' <- lintExpr val
  if mTypeOf name == val'
    then modify (name:) >> lintExpr body
    else throwError $ pPrint val <+> "cannot assign to:" <+> (pPrint name <> ":" <> pPrint (mTypeOf name))
lintExpr e@(Apply f args) = do
  mapM_ defined (f:args)
  paramtys <- getParamtys
  if paramtys /= argtys
    then throwError ("expected:" <+> pPrint paramtys <> ","
                     $+$ "actual:" <+> pPrint argtys
                     $+$ "code:" <+> pPrint e)
    else return (mTypeOf e)
  where fty = mTypeOf f
        argtys = map mTypeOf args
        getParamtys =
          case fty of
            FunctionTy _ ts -> return ts
            -- PointerTy (StructTy [FunctionTy _ ts, _]) -> return ts
            t -> throwError $ pPrint t <+> ("is not applieable: " <> parens (pPrint e))
lintExpr (Access e is) =
  defined e >> accessMType (mTypeOf e) is
lintExpr (If c t f)
  | mTypeOf c == IntTy 1 = do
      defined c
      t' <- lintExpr t
      f' <- lintExpr f
      if t' == f'
        then return t'
        else throwError $ pPrint t <+> "must be typed as:" <+> pPrint f'
  | otherwise = throwError $ pPrint c <+> "must be typed as: i1"
lintExpr (Var a) =
  defined a >> return (mTypeOf a)
lintExpr e@(Tuple xs) =
  mapM_ defined xs >> return (mTypeOf e)
lintExpr (LetRec fundecs body) = do
  modify (map (view _1) fundecs ++)
  mapM_ lintFunDec fundecs
  lintExpr body
lintExpr (Cast ty a) =
  defined a >> return ty
lintExpr e = return $ mTypeOf e

lintFunDec :: (MonadState [a] m, Eq a, HasMType a, MonadError Doc m, Pretty a) => (a, Maybe [a], Expr a) -> m ()
lintFunDec (_, mparams, fbody) =
  modify (fromMaybe [] mparams ++) >> void (lintExpr fbody)

lintDefn :: (MonadError Doc m, HasMType a, Eq a, MonadState [a] m, Pretty a) => Defn a -> m ()
lintDefn (DefFun _ params fbody) =
  modify (params ++) >> void (lintExpr fbody)

lintProgram :: (MonadState [a] m, MonadError Doc m, HasMType a, Eq a, Pretty a) => Program a -> m ()
lintProgram (Program _ xs) = do
  modify (map (\(DefFun f _ _) -> f) xs ++)
  mapM_ lintDefn xs
