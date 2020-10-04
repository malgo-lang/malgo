{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Core.Lint
  ( lint,
  )
where

import Control.Monad.Except
import Koriel.Prelude
import Koriel.Pretty
import Language.Malgo.IR.Core
import Language.Malgo.IR.Op
import Language.Malgo.Id
import Language.Malgo.TypeRep.CType

lint :: (Monad m, HasCType a, Pretty a) => Exp (Id a) -> m ()
lint e =
  runExceptT (runReaderT (lintExp e) []) >>= \case
    Left err -> errorDoc err
    Right e' -> pure e'

defined :: (MonadReader (t a) m, Foldable t, Eq a, MonadError Doc m, Pretty a) => a -> m ()
defined x = do
  env <- ask
  unless (x `elem` env) $ throwError $ pPrint x <> " is not defined"

match ::
  (HasCType a, HasCType b, MonadError Doc f, Pretty a, Pretty b, HasCallStack) =>
  a ->
  b ->
  f ()
match (cTypeOf -> ps0 :-> r0) (cTypeOf -> ps1 :-> r1) = do
  zipWithM_ match ps0 ps1
  match r0 r1
match (cTypeOf -> DataT {}) (cTypeOf -> AnyT) = pure ()
match (cTypeOf -> AnyT) (cTypeOf -> DataT {}) = pure ()
match (cTypeOf -> AnyT) (cTypeOf -> AnyT) = pure ()
match x y
  | cTypeOf x == cTypeOf y =
    pure ()
  | otherwise =
    throwError $
      "type mismatch:"
        $$ (pPrint x <+> ":" <+> pPrint (cTypeOf x))
        $$ (pPrint y <+> ":" <+> pPrint (cTypeOf y))

lintExp :: (MonadReader [Id a] m, HasCType a, Pretty a, MonadError Doc m) => Exp (Id a) -> m ()
lintExp (Atom x) = lintAtom x
lintExp (Call f xs) = do
  lintAtom f
  traverse_ lintAtom xs
  case cTypeOf f of
    ps :-> r -> match f (map cTypeOf xs :-> r) >> zipWithM_ match ps xs
    _ -> throwError $ pPrint f <+> "is not callable"
lintExp (CallDirect f xs) = do
  defined f
  traverse_ lintAtom xs
  case cTypeOf f of
    ps :-> r -> match f (map cTypeOf xs :-> r) >> zipWithM_ match ps xs
    _ -> throwError $ pPrint f <+> "is not callable"
lintExp (PrimCall _ (ps :-> _) xs) = do
  traverse_ lintAtom xs
  zipWithM_ match ps xs
lintExp PrimCall {} = throwError "primitive must be a function"
lintExp (BinOp o x y) = do
  lintAtom x
  lintAtom y
  case o of
    Add -> (match Int32T x >> match Int32T y) `catchError` const (match Int64T x >> match Int64T y)
    Sub -> (match Int64T x >> match Int64T y) `catchError` const (match Int64T x >> match Int64T y)
    Mul -> (match Int64T x >> match Int64T y) `catchError` const (match Int64T x >> match Int64T y)
    Div -> (match Int64T x >> match Int64T y) `catchError` const (match Int64T x >> match Int64T y)
    Mod -> (match Int64T x >> match Int64T y) `catchError` const (match Int64T x >> match Int64T y)
    FAdd ->
      (match FloatT x >> match FloatT y) `catchError` const (match DoubleT x >> match DoubleT y)
    FSub ->
      (match FloatT x >> match FloatT y) `catchError` const (match DoubleT x >> match DoubleT y)
    FMul ->
      (match FloatT x >> match FloatT y) `catchError` const (match DoubleT x >> match DoubleT y)
    FDiv ->
      (match FloatT x >> match FloatT y) `catchError` const (match DoubleT x >> match DoubleT y)
    Eq -> match x y
    Neq -> match x y
    Lt -> match x y
    Le -> match x y
    Gt -> match x y
    Ge -> match x y
    _ -> throwError "And and Or is not supported"
lintExp (ArrayRead a i) = do
  lintAtom a
  lintAtom i
  case cTypeOf a of
    ArrayT _ -> match Int64T i
    _ -> throwError $ pPrint a <+> "must be a array"
lintExp (ArrayWrite a i v) = do
  lintAtom a
  lintAtom i
  lintAtom v
  case cTypeOf a of
    ArrayT t -> match Int64T i >> match t v
    _ -> throwError $ pPrint a <+> "must be a array"
lintExp (Cast _ x) = lintAtom x
lintExp (Let ds e) = local (map fst ds <>) $ do
  traverse_ (lintObj . snd) ds
  lintExp e
lintExp (Match e cs) = do
  lintExp e
  traverse_ lintCase cs
lintExp Error {} = pure ()

lintObj :: (MonadReader [Id a] m, MonadError Doc m, Pretty a, HasCType a) => Obj (Id a) -> m ()
lintObj (Fun params body) = local (params <>) $ lintExp body
lintObj (Pack _ _ xs) = traverse_ lintAtom xs
lintObj (Array a n) = lintAtom a >> lintAtom n >> match Int64T n

lintCase :: (MonadReader [Id a] m, MonadError Doc m, Pretty a, HasCType a) => Case (Id a) -> m ()
lintCase (Unpack _ vs e) = local (vs <>) $ lintExp e
lintCase (Switch _ e) = lintExp e
lintCase (Bind x e) = local (x :) $ lintExp e

lintAtom :: (MonadReader [Id a] m, MonadError Doc m, Pretty a) => Atom (Id a) -> m ()
lintAtom (Var x) = defined x
lintAtom (Unboxed _) = pure ()
