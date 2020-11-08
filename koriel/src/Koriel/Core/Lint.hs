{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Koriel.Core.Lint
  ( lint,
    runLint,
    lintProgram,
  )
where

import Control.Monad.Except
import Koriel.Core.Core
import Koriel.Core.Op
import Koriel.Core.Type
import Koriel.Id
import Koriel.Prelude
import Koriel.Pretty

runLint :: Monad m => ReaderT [a] (ExceptT Doc m) b -> m b
runLint m =
  runExceptT (runReaderT m []) >>= \case
    Left err -> errorDoc err
    Right e' -> pure e'

lint :: (Monad m, HasType a, Pretty a) => Exp (Id a) -> m ()
lint e =
  runExceptT (runReaderT (lintExp e) []) >>= \case
    Left err -> errorDoc err
    Right e' -> pure e'

defined :: (MonadReader (t a) m, Foldable t, Eq a, MonadError Doc m, Pretty a) => a -> m ()
defined x = do
  env <- ask
  unless (x `elem` env) $ throwError $ pPrint x <> " is not defined"

match ::
  (HasType a, HasType b, MonadError Doc f, Pretty a, Pretty b, HasCallStack) =>
  a ->
  b ->
  f ()
match (typeOf -> ps0 :-> r0) (typeOf -> ps1 :-> r1) = do
  zipWithM_ match ps0 ps1
  match r0 r1
match (typeOf -> DataT {}) (typeOf -> AnyT) = pure ()
match (typeOf -> AnyT) (typeOf -> DataT {}) = pure ()
match (typeOf -> AnyT) (typeOf -> AnyT) = pure ()
match x y
  | typeOf x == typeOf y =
    pure ()
  | otherwise =
    throwError $
      "type mismatch:"
        $$ (pPrint x <+> ":" <+> pPrint (typeOf x))
        $$ (pPrint y <+> ":" <+> pPrint (typeOf y))

lintExp :: (MonadReader [Id a] m, HasType a, Pretty a, MonadError Doc m) => Exp (Id a) -> m ()
lintExp (Atom x) = lintAtom x
lintExp (Call f xs) = do
  lintAtom f
  traverse_ lintAtom xs
  case typeOf f of
    ps :-> r -> match f (map typeOf xs :-> r) >> zipWithM_ match ps xs
    _ -> throwError $ pPrint f <+> "is not callable"
lintExp (CallDirect f xs) = do
  defined f
  traverse_ lintAtom xs
  case typeOf f of
    ps :-> r -> match f (map typeOf xs :-> r) >> zipWithM_ match ps xs
    _ -> throwError $ pPrint f <+> "is not callable"
lintExp (ExtCall _ (ps :-> _) xs) = do
  traverse_ lintAtom xs
  zipWithM_ match ps xs
lintExp ExtCall {} = throwError "primitive must be a function"
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
    And -> match x BoolT >> match y BoolT
    Or -> match x BoolT >> match y BoolT
lintExp (ArrayRead a i) = do
  lintAtom a
  lintAtom i
  case typeOf a of
    ArrayT _ -> match Int64T i
    _ -> throwError $ pPrint a <+> "must be a array"
lintExp (ArrayWrite a i v) = do
  lintAtom a
  lintAtom i
  lintAtom v
  case typeOf a of
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

lintObj :: (MonadReader [Id a] m, MonadError Doc m, Pretty a, HasType a) => Obj (Id a) -> m ()
lintObj (Fun params body) = local (params <>) $ lintExp body
lintObj (Pack _ _ xs) = traverse_ lintAtom xs
lintObj (Array a n) = lintAtom a >> lintAtom n >> match Int64T n

lintCase :: (MonadReader [Id a] m, MonadError Doc m, Pretty a, HasType a) => Case (Id a) -> m ()
lintCase (Unpack _ vs e) = local (vs <>) $ lintExp e
lintCase (Switch _ e) = lintExp e
lintCase (Bind x e) = local (x :) $ lintExp e

lintAtom :: (MonadReader [Id a] m, MonadError Doc m, Pretty a) => Atom (Id a) -> m ()
lintAtom (Var x) = defined x
lintAtom (Unboxed _) = pure ()

lintProgram :: (MonadReader [Id a] m, HasType a, Pretty a, MonadError Doc m) => Program (Id a) -> m ()
lintProgram (Program funcs e) = do
  let fs = map (view _1) funcs
  local (fs <>) $ do
    traverse_ (\(_, (ps, body)) -> local (ps <>) $ lintExp body) funcs
    lintExp e