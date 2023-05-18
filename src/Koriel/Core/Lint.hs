module Koriel.Core.Lint (lint) where

import Control.Lens (has, traverseOf_, traversed, view, _1, _2)
import Data.HashMap.Strict qualified as HashMap
import Data.String.Conversions (convertString)
import Koriel.Core.Op
import Koriel.Core.Syntax
import Koriel.Core.Type
import Koriel.Id
import Koriel.Prelude
import Koriel.Pretty

-- | Lint a program.
-- The reason `lint` is a monadic action is to control when errors are reported.
lint :: HasCallStack => Monad m => Program (Id Type) -> m ()
lint = runLint . lintProgram

runLint :: ReaderT [Id Type] m a -> m a
runLint m = runReaderT m []

defined :: HasCallStack => MonadReader [Id Type] f => Id Type -> f ()
defined x
  | idIsExternal x = pass
  | otherwise = do
      env <- ask
      unless (x `elem` env) $ errorDoc $ pPrint x <> " is not defined"

define :: HasCallStack => MonadReader [Id Type] f => Doc -> [Id Type] -> f a -> f a
define pos xs m = do
  env <- ask
  for_ xs \x ->
    when (x `elem` env) $
      errorDoc $
        pPrint x <> " is already defined"
          $$ "while defining"
          <+> pos
          <+> pPrint xs
  local (xs <>) m

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

lintExpr :: HasCallStack => MonadReader [Id Type] m => Expr (Id Type) -> m ()
lintExpr (Atom x) = lintAtom x
lintExpr (Call f xs) = do
  lintAtom f
  traverse_ lintAtom xs
  case typeOf f of
    ps :-> r -> match f (map typeOf xs :-> r) >> zipWithM_ match ps xs
    _ -> errorDoc $ pPrint f <+> "is not callable"
lintExpr (CallDirect f xs) = do
  defined f
  traverse_ lintAtom xs
  case typeOf f of
    ps :-> r -> match f (map typeOf xs :-> r) >> zipWithM_ match ps xs
    _ -> errorDoc $ pPrint f <+> "is not callable"
lintExpr (RawCall _ (ps :-> _) xs) = do
  traverse_ lintAtom xs
  zipWithM_ match ps xs
lintExpr RawCall {} = error "primitive must be a function"
lintExpr (BinOp o x y) = do
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
lintExpr (Cast _ x) = lintAtom x
lintExpr (Let ds e) = define "let" (map (._variable) ds) $ do
  traverse_ (lintObj . (._object)) ds
  for_ ds $ \LocalDef {_variable, _object} -> match _variable _object
  lintExpr e
lintExpr (Match e cs) = do
  lintExpr e
  traverse_ (lintCase e) cs
  -- check if all cases have same type of pattern
  if all (\c -> has _Unpack c || has _Bind c) cs
    || all (\c -> has _OpenRecord c || has _Bind c) cs
    || all (\c -> has _Exact c || has _Bind c) cs
    then pass
    else errorDoc $ "pattern mismatch:" $$ pPrint cs
lintExpr (Switch a cs e) = do
  lintAtom a
  traverseOf_ (traversed . _2) lintExpr cs
  lintExpr e
lintExpr (SwitchUnboxed a cs e) = do
  lintAtom a
  traverseOf_ (traversed . _2) lintExpr cs
  lintExpr e
lintExpr (Destruct a _ xs e) = do
  lintAtom a
  define "destruct" xs $ lintExpr e
lintExpr (DestructRecord a xs e) = do
  lintAtom a
  define "destruct-record" (HashMap.elems xs) $ lintExpr e
lintExpr (Assign x (Atom v) _) = do
  errorDoc $ "reduntant assignment:" <+> pPrint x <+> pPrint v
lintExpr (Assign x v e) = do
  lintExpr v
  define "assign" [x] (lintExpr e)
lintExpr Error {} = pass

lintObj :: HasCallStack => MonadReader [Id Type] m => Obj (Id Type) -> m ()
lintObj (Fun params body) = define "fun" params $ lintExpr body
lintObj (Pack _ _ xs) = traverse_ lintAtom xs
lintObj (Record kvs) = traverse_ lintAtom kvs

lintCase :: HasCallStack => MonadReader [Id Type] m => Expr (Id Type) -> Case (Id Type) -> m ()
lintCase _ (Unpack _ vs e) = define "unpack" vs $ lintExpr e
lintCase _ (OpenRecord kvs e) = define "open-record" (HashMap.elems kvs) $ lintExpr e
lintCase _ (Exact _ e) = lintExpr e
lintCase scrutinee (Bind x t e) = define "bind" [x] do
  match x t
  match scrutinee x
  lintExpr e

lintAtom :: HasCallStack => MonadReader [Id Type] m => Atom (Id Type) -> m ()
lintAtom (Var x) = defined x
lintAtom (Unboxed _) = pass

lintProgram :: HasCallStack => MonadReader [Id Type] m => Program (Id Type) -> m ()
lintProgram Program {..} = do
  let vs = map (view _1) topVars
  let fs = map (view _1) topFuns
  define "program" (vs <> fs) do
    for_ topVars \(v, _, e) -> do
      match v (typeOf e)
      lintExpr e
    for_ topFuns \(f, ps, _, body) -> define (pPrint f) ps do
      match f (map typeOf ps :-> typeOf body)
      lintExpr body
