{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Malgo.MiddleEnd.BasicLint (lint, runLint, lintExpr, lintProgram) where

import           Language.Malgo.ID
import           Language.Malgo.IR.IR
import           Language.Malgo.Prelude

type BasicLint ann a = StateT [ID MType] (Except (Doc ann)) a

lint :: Expr (ID MType) -> Either (Doc ann) MType
lint expr = runExcept $ evalStateT (lintExpr expr) []

runLint :: StateT [a1] (ExceptT e Identity) a2 -> Either e a2
runLint = runExcept . flip evalStateT []

defined :: ID MType -> BasicLint ann ()
defined a =
  ifM (elem a <$> get)
  (return ())
  (throwError $ pretty a <+> "is not defined")

notDefined :: ID MType -> BasicLint ann ()
notDefined a =
  ifM (notElem a <$> get)
  (return ())
  (throwError $ pretty a <+> "is already defined")

lintExpr :: Expr (ID MType) -> BasicLint ann MType
lintExpr (Let name val body) = do
  notDefined name
  val' <- lintExpr val
  if mTypeOf name == val'
    then modify (name:) >> lintExpr body
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
            -- PointerTy (StructTy [FunctionTy _ ts, _]) -> return ts
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
lintExpr (LetRec fundecs body) = do
  modify (map (view _1) fundecs ++)
  mapM_ lintFunDec fundecs
  lintExpr body
lintExpr (Cast ty a) =
  defined a >> return ty
lintExpr e = return $ mTypeOf e

lintFunDec :: (ID MType, Maybe [ID MType], Expr (ID MType)) -> BasicLint ann ()
lintFunDec (_, mparams, fbody) =
  modify (fromMaybe [] mparams ++) >> void (lintExpr fbody)

lintDefn :: Defn (ID MType) -> BasicLint ann ()
lintDefn (DefFun _ params fbody) =
  modify (params ++) >> void (lintExpr fbody)

lintProgram :: Program (ID MType) -> BasicLint ann ()
lintProgram (Program xs) = do
  modify (map (\(DefFun f _ _) -> f) xs ++)
  mapM_ lintDefn xs
