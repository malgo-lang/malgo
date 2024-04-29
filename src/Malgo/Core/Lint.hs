module Malgo.Core.Lint (lint) where

import Control.Lens (has, traverseOf_, traversed, view, _1, _2)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader, ask, asks, local, runReader)
import Malgo.Core.Syntax
import Malgo.Core.Type
import Malgo.Id
import Malgo.Prelude

-- | Lint a program.
-- The reason `lint` is a monadic action is to control when errors are reported.
lint :: Bool -> Program (Meta Type) -> Eff es ()
lint normalized = runLint normalized . lintProgram

data LintEnv = LintEnv
  { defs :: HashSet (Meta Type),
    normalized :: Bool,
    isIncludeAssign :: Bool,
    isStatement :: Bool
  }

runLint :: Bool -> Eff (Reader LintEnv : es) a -> Eff es a
runLint normalized = runReader (LintEnv mempty normalized True True)

asStatement :: (Reader LintEnv :> es) => Eff es a -> Eff es a
asStatement = local (\e -> e {isStatement = True})

statement :: (Reader LintEnv :> es, Pretty a) => a -> Eff es b -> Eff es b
statement e m = do
  LintEnv {isStatement, normalized} <- ask
  if
    | not normalized -> m
    | isStatement -> m
    | otherwise -> errorDoc $ pretty e <+> "must be a statement"

defined :: (Reader LintEnv :> es) => Meta Type -> Eff es ()
defined x
  | idIsExternal x.id = pass
  | otherwise = do
      env <- asks @LintEnv (.defs)
      unless (HashSet.member x env) $ errorDoc $ pretty x <> " is not defined"

define :: (Reader LintEnv :> es) => Doc x -> [Meta Type] -> Eff es a -> Eff es a
define pos xs m = do
  env <- asks @LintEnv (.defs)
  for_ xs \x ->
    when (HashSet.member x env) $
      errorDoc $
        vsep
          [ pretty x
              <> " is already defined",
            "while defining"
              <+> pos
              <+> pretty xs
          ]
  local (\e -> e {defs = HashSet.fromList xs <> e.defs}) m

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
        vsep
          [ "type mismatch:",
            pretty x,
            nest 2 (":" <> pretty (typeOf x)),
            pretty y,
            nest 2 (":" <> pretty (typeOf y))
          ]

lintExpr :: (Reader LintEnv :> es) => Expr (Meta Type) -> Eff es ()
lintExpr (Atom x) = lintAtom x
lintExpr (Call f xs) = do
  lintAtom f
  traverse_ lintAtom xs
  case typeOf f of
    ps :-> r -> match f (map typeOf xs :-> r) >> zipWithM_ match ps xs
    _ -> errorDoc $ pretty f <+> "is not callable"
lintExpr (CallDirect f xs) = do
  defined f
  traverse_ lintAtom xs
  case typeOf f of
    ps :-> r -> match f (map typeOf xs :-> r) >> zipWithM_ match ps xs
    _ -> errorDoc $ pretty f <+> "is not callable"
lintExpr (RawCall _ (ps :-> _) xs) = do
  traverse_ lintAtom xs
  zipWithM_ match ps xs
lintExpr RawCall {} = error "primitive must be a function"
lintExpr (Cast _ x) = lintAtom x
lintExpr (Let ds e) = local (\e -> e {isIncludeAssign = True}) $
  define "let" (map (._variable) ds) do
    traverse_ (lintObj . (._object)) ds
    for_ ds $ \LocalDef {_variable, _object} -> match _variable _object
    asStatement $ lintExpr e
lintExpr (Match e cs) = do
  LintEnv {normalized} <- ask
  when normalized $ errorDoc "match is not allowed"
  lintExpr e
  local (\e -> e {isIncludeAssign = True}) $ traverse_ (lintCase e) cs
  -- check if all cases have same type of pattern
  if all (\c -> has _Unpack c || has _Bind c) cs
    || all (\c -> has _OpenRecord c || has _Bind c) cs
    || all (\c -> has _Exact c || has _Bind c) cs
    then pass
    else errorDoc $ vsep ["pattern mismatch:", pretty cs]
lintExpr (Switch a cs e) = statement (Switch a cs e) do
  lintAtom a
  traverseOf_ (traversed . _2) lintExpr cs
  lintExpr e
lintExpr (SwitchUnboxed a cs e) = statement (SwitchUnboxed a cs e) do
  lintAtom a
  traverseOf_ (traversed . _2) lintExpr cs
  lintExpr e
lintExpr x@(Destruct a _ xs e) = statement x do
  lintAtom a
  define "destruct" xs $ lintExpr e
lintExpr x@(DestructRecord a xs e) = statement x do
  lintAtom a
  define "destruct-record" (HashMap.elems xs) $ lintExpr e
lintExpr (Assign x (Atom v) _) = do
  errorDoc $ "reduntant assignment:" <+> pretty x <+> pretty v
lintExpr (Assign x v e) = statement (Assign x v e) do
  LintEnv {isIncludeAssign, normalized} <- ask
  if normalized && not isIncludeAssign
    then errorDoc "assignment is not allowed"
    else do
      local (\e -> e {isIncludeAssign = False, isStatement = False}) $ lintExpr v
      define "assign" [x] (lintExpr e)
lintExpr Error {} = pass

lintObj :: (Reader LintEnv :> es) => Obj (Meta Type) -> Eff es ()
lintObj (Fun params body) = define "fun" params $ asStatement $ lintExpr body
lintObj (Pack _ _ xs) = traverse_ lintAtom xs
lintObj (Record kvs) = traverse_ lintAtom kvs

lintCase :: (Reader LintEnv :> es, HasType a, Pretty a) => a -> Case (Meta Type) -> Eff es ()
lintCase _ (Unpack _ vs e) = define "unpack" vs $ lintExpr e
lintCase _ (OpenRecord kvs e) = define "open-record" (HashMap.elems kvs) $ lintExpr e
lintCase _ (Exact _ e) = lintExpr e
lintCase scrutinee (Bind x t e) = define "bind" [x] do
  match x t
  match scrutinee x
  lintExpr e

lintAtom :: (Reader LintEnv :> es) => Atom (Meta Type) -> Eff es ()
lintAtom (Var x) = defined x
lintAtom (Unboxed _) = pass

lintProgram :: (Reader LintEnv :> es) => Program (Meta Type) -> Eff es ()
lintProgram Program {..} = do
  let vs = map (view _1) topVars
  let fs = map (view _1) topFuns
  define "program" (vs <> fs) do
    for_ topVars \(v, _, e) -> do
      match v (typeOf e)
      lintExpr e
    for_ topFuns \(f, ps, _, body) -> define (pretty f) ps do
      match f (map typeOf ps :-> typeOf body)
      lintExpr body
