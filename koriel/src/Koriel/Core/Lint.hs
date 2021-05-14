{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Koriel.Core.Lint
  ( lint,
    runLint,
    lintProgram,
  )
where

import Control.Monad.Except
import Koriel.Core.Op
import Koriel.Core.Syntax
import Koriel.Core.Type
import Koriel.Id
import Koriel.Prelude
import Koriel.Pretty

runLint :: ReaderT [a] m b -> m b
runLint m = runReaderT m []

lint :: (Monad m, HasType a, Pretty a, Eq a) => Exp (Id a) -> m ()
lint e =
  runReaderT (lintExp e) []

defined :: (MonadReader (t (Id a)) f, Foldable t, Eq a, Pretty a) => Id a -> f ()
defined x
  | idIsExternal x = pure ()
  | otherwise = do
    env <- ask
    unless (x `elem` env) $ errorDoc $ pPrint x <> " is not defined"

isMatch :: (HasType a, HasType b) => a -> b -> Bool
isMatch (typeOf -> ps0 :-> r0) (typeOf -> ps1 :-> r1) =
  and (zipWith isMatch ps0 ps1) && isMatch r0 r1
isMatch (typeOf -> AnyT) (typeOf -> AnyT) = True
isMatch x y
  | typeOf x == typeOf y = True
  | otherwise = False

match :: (HasType a, HasType b, Pretty a, Pretty b, Applicative f) => a -> b -> f ()
match x y
  | isMatch x y = pure ()
  | otherwise =
    errorDoc $
      "type mismatch:"
        $$ pPrint x <> ":" <> pPrint (typeOf x)
        $$ pPrint y <> ":" <> pPrint (typeOf y)

lintExp :: (MonadReader [Id a] m, HasType a, Eq a, Pretty a) => Exp (Id a) -> m ()
lintExp (Atom x) = lintAtom x
lintExp (Call f xs) = do
  lintAtom f
  traverse_ lintAtom xs
  case typeOf f of
    ps :-> r -> match f (map typeOf xs :-> r) >> zipWithM_ match ps xs
    _ -> errorDoc $ pPrint f <+> "is not callable"
lintExp (CallDirect f xs) = do
  defined f
  traverse_ lintAtom xs
  case typeOf f of
    ps :-> r -> match f (map typeOf xs :-> r) >> zipWithM_ match ps xs
    _ -> errorDoc $ pPrint f <+> "is not callable"
lintExp (ExtCall _ (ps :-> _) xs) = do
  traverse_ lintAtom xs
  zipWithM_ match ps xs
lintExp ExtCall {} = error "primitive must be a function"
lintExp (BinOp o x y) = do
  lintAtom x
  lintAtom y
  case o of
    Add
      | isMatch x Int32T -> match x y
      | isMatch x Int64T -> match x y
      | otherwise -> errorDoc $ "type mismatch:" $$ pPrint x <> ":" <> pPrint (typeOf x) $$ pPrint [Int32T, Int64T]
    Sub
      | isMatch x Int32T -> match x y
      | isMatch x Int64T -> match x y
      | otherwise -> errorDoc $ "type mismatch:" $$ pPrint x <> ":" <> pPrint (typeOf x) $$ pPrint [Int32T, Int64T]
    Mul
      | isMatch x Int32T -> match x y
      | isMatch x Int64T -> match x y
      | otherwise -> errorDoc $ "type mismatch:" $$ pPrint x <> ":" <> pPrint (typeOf x) $$ pPrint [Int32T, Int64T]
    Div
      | isMatch x Int32T -> match x y
      | isMatch x Int64T -> match x y
      | otherwise -> errorDoc $ "type mismatch:" $$ pPrint x <> ":" <> pPrint (typeOf x) $$ pPrint [Int32T, Int64T]
    Mod
      | isMatch x Int32T -> match x y
      | isMatch x Int64T -> match x y
      | otherwise -> errorDoc $ "type mismatch:" $$ pPrint x <> ":" <> pPrint (typeOf x) $$ pPrint [Int32T, Int64T]
    FAdd
      | isMatch x FloatT -> match x y
      | isMatch x DoubleT -> match x y
      | otherwise -> errorDoc $ "type mismatch:" $$ pPrint x <> ":" <> pPrint (typeOf x) $$ pPrint [FloatT, DoubleT]
    FSub
      | isMatch x FloatT -> match x y
      | isMatch x DoubleT -> match x y
      | otherwise -> errorDoc $ "type mismatch:" $$ pPrint x <> ":" <> pPrint (typeOf x) $$ pPrint [FloatT, DoubleT]
    FMul
      | isMatch x FloatT -> match x y
      | isMatch x DoubleT -> match x y
      | otherwise -> errorDoc $ "type mismatch:" $$ pPrint x <> ":" <> pPrint (typeOf x) $$ pPrint [FloatT, DoubleT]
    FDiv
      | isMatch x FloatT -> match x y
      | isMatch x DoubleT -> match x y
      | otherwise -> errorDoc $ "type mismatch:" $$ pPrint x <> ":" <> pPrint (typeOf x) $$ pPrint [FloatT, DoubleT]
    Eq -> match x y
    Neq -> match x y
    Lt -> match x y
    Le -> match x y
    Gt -> match x y
    Ge -> match x y
    And -> match x BoolT >> match y BoolT
    Or -> match x BoolT >> match y BoolT
lintExp (Cast _ x) = lintAtom x
lintExp (Let ds e) = local (map (view localDefVar) ds <>) $ do
  traverse_ (lintObj . view localDefObj) ds
  lintExp e
lintExp (Match e cs) = do
  lintExp e
  traverse_ lintCase cs
lintExp Error {} = pure ()

lintObj :: (MonadReader [Id a] m, Pretty a, HasType a, Eq a) => Obj (Id a) -> m ()
lintObj (Fun params body) = local (params <>) $ lintExp body
lintObj (Pack _ _ xs) = traverse_ lintAtom xs

lintCase :: (MonadReader [Id a] m, Pretty a, HasType a, Eq a) => Case (Id a) -> m ()
lintCase (Unpack _ vs e) = local (vs <>) $ lintExp e
lintCase (Switch _ e) = lintExp e
lintCase (Bind x e) = local (x :) $ lintExp e

lintAtom :: (MonadReader [Id a] m, Pretty a, Eq a) => Atom (Id a) -> m ()
lintAtom (Var x) = defined x
lintAtom (Unboxed _) = pure ()

lintProgram :: (MonadReader [Id a] m, HasType a, Pretty a, Eq a) => Program (Id a) -> m ()
lintProgram Program {..} = do
  let fs = map (view _1) _topFuncs
  local (fs <>) $
    traverse_ ?? _topFuncs $ \(f, (ps, body)) -> local (ps <>) do
      match f (map typeOf ps :-> typeOf body)
      lintExp body