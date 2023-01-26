module Koriel.Core.Lint (lint) where

import Control.Lens (view, _1)
import Control.Monad.Except
import Data.HashMap.Strict qualified as HashMap
import Koriel.Core.Op
import Koriel.Core.Syntax
import Koriel.Core.Type
import Koriel.Id
import Koriel.Prelude
import Koriel.Pretty

-- | Lint a program.
-- The reason `lint` is a monadic action is to control when errors are reported.
lint :: Monad m => Program (Id Type) -> m ()
lint = runLint . lintProgram

runLint :: ReaderT [Id Type] m a -> m a
runLint m = runReaderT m []

defined :: (MonadReader [Id Type] f) => Id Type -> f ()
defined x
  | idIsExternal x = pass
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
  | isMatch x y = pass
  | otherwise =
      errorDoc $
        "type mismatch:"
          $$ pPrint x
          $$ nest 2 (":" <> pPrint (typeOf x))
          $$ pPrint y
          $$ nest 2 (":" <> pPrint (typeOf y))

lintExp :: MonadReader [Id Type] m => Exp (Id Type) -> m ()
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
-- lintExp (ExtCall _ (ps :-> _) xs) = do
--   traverse_ lintAtom xs
--   zipWithM_ match ps xs
-- lintExp ExtCall {} = error "primitive must be a function"
lintExp (RawCall _ (ps :-> _) xs) = do
  traverse_ lintAtom xs
  zipWithM_ match ps xs
lintExp RawCall {} = error "primitive must be a function"
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
lintExp (Let ds e) = local (map (._variable) ds <>) $ do
  traverse_ (lintObj . (._object)) ds
  for_ ds $ \LocalDef {_variable, _object} -> match _variable _object
  lintExp e
lintExp (Match e cs) = do
  lintExp e
  traverse_ (lintCase e) cs
lintExp Error {} = pass

lintObj :: MonadReader [Id Type] m => Obj (Id Type) -> m ()
lintObj (Fun params body) = local (params <>) $ lintExp body
lintObj (Pack _ _ xs) = traverse_ lintAtom xs
lintObj (Record kvs) = traverse_ lintAtom kvs

lintCase :: MonadReader [Id Type] m => Exp (Id Type) -> Case (Id Type) -> m ()
lintCase _ (Unpack _ vs e) = local (vs <>) $ lintExp e
lintCase _ (OpenRecord kvs e) = local (HashMap.elems kvs <>) $ lintExp e
lintCase _ (Switch _ e) = lintExp e
lintCase scrutinee (Bind x e) = local (x :) do
  match scrutinee x
  lintExp e

lintAtom :: MonadReader [Id Type] m => Atom (Id Type) -> m ()
lintAtom (Var x) = defined x
lintAtom (Unboxed _) = pass

lintProgram :: MonadReader [Id Type] m => Program (Id Type) -> m ()
lintProgram Program {..} = do
  let fs = map (view _1) topFuncs
  local (fs <>) $
    for_ topFuncs $ \(f, (ps, body)) -> local (ps <>) do
      match f (map typeOf ps :-> typeOf body)
      lintExp body