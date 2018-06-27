{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Malgo.MiddleEnd.BasicLint (lintExpr) where

import           Language.Malgo.ID
import           Language.Malgo.IR.IR
import           Language.Malgo.Prelude

defined :: [ID MType] -> ID MType -> Either (Doc ann) ()
defined env a | a `elem` env = Right ()
              | otherwise = Left $ pretty a <+> "is not defined"

notDefined :: [ID MType] -> ID MType -> Either (Doc ann) ()
notDefined env a = case defined env a of
  Right () -> Left $ pretty a <+> "is already defined"
  Left _ -> Right ()

lintExpr :: [ID MType] -> Expr (ID MType) -> Either (Doc ann) MType
lintExpr env (Let name val body) = do
  notDefined env name
  val' <- lintExpr env val
  if mTypeOf name == val'
    then lintExpr (name:env) body
    else throwError $ pretty val <+> "cannot assign to:" <+> pretty name <> ":" <> pretty (mTypeOf name)
lintExpr env e@(Apply f args) = do
  mapM_ (defined env) (f:args)
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
lintExpr env (Access e is) =
  defined env e >> accessMType (mTypeOf e) is
lintExpr env (If c t f)
  | mTypeOf c == IntTy 1 = do
      defined env c
      t' <- lintExpr env t
      f' <- lintExpr env f
      if t' == f'
        then return t'
        else throwError $ pretty t <+> "must be typed as:" <+> pretty f'
  | otherwise = throwError $ pretty c <+> "must be typed as: i1"
lintExpr env (Var a) =
  defined env a >> return (mTypeOf a)
lintExpr env e@(Tuple xs) =
  mapM_ (defined env) xs >> return (mTypeOf e)
lintExpr env (LetRec fundecs body) = do
  let env' = map (view _1) fundecs ++ env
  mapM_ (lintFunDec env') fundecs
  lintExpr env' body
lintExpr env (Cast ty a) =
  defined env a >> return ty
lintExpr _ e = return $ mTypeOf e

lintFunDec :: [ID MType] -> (ID MType, Maybe [ID MType], Expr (ID MType)) -> Either (Doc ann) ()
lintFunDec env (_, mparams, fbody) = do
  let env' = fromMaybe [] mparams ++ env
  void $ lintExpr env' fbody
