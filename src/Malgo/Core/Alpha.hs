-- | Alpha conversion
module Malgo.Core.Alpha
  ( alpha,
    equiv,
    Subst,
  )
where

import Control.Exception (assert)
import Control.Lens (traverseOf)
import Data.Map.Strict qualified as Map
import Effectful (Eff, runPureEff, (:>))
import Effectful.Reader.Static (Reader, ask, local, runReader)
import Effectful.State.Static.Local (State)
import Malgo.Core.Syntax
import Malgo.Core.Type
import Malgo.Id
import Malgo.Module
import Malgo.MonadUniq
import Malgo.Prelude

type Subst = Map (Meta Type) (Atom (Meta Type))

-- | Alpha conversion
alpha ::
  (Reader ModuleName :> es, State Uniq :> es) =>
  Subst ->
  Expr (Meta Type) ->
  Eff es (Expr (Meta Type))
alpha s x = runAlpha s $ alphaExpr x

runAlpha :: Subst -> Eff (Reader Subst : es) a -> Eff es a
runAlpha = runReader

lookupVar :: (Reader Subst :> es) => Meta Type -> Eff es (Atom (Meta Type))
lookupVar n =
  fromMaybe (Var n) . Map.lookup n <$> ask

lookupId :: (Reader Subst :> es) => Meta Type -> Eff es (Meta Type)
lookupId n = do
  n' <- lookupVar n
  case n' of
    Var i -> pure i
    _ -> error $ show n <> " must be bound to Var"

cloneMeta :: (Reader ModuleName :> es, State Uniq :> es) => Meta a -> Eff es (Meta a)
cloneMeta Meta {meta, id = Id {..}} = do
  assert (case sort of Internal _ -> True; Temporal _ -> True; _ -> False) pass
  moduleName <- ask @ModuleName
  uniq <- getUniq
  pure case sort of
    Internal _ -> Meta {meta, id = Id {sort = Internal uniq, ..}}
    Temporal _ -> Meta {meta, id = Id {sort = Temporal uniq, ..}}
    _ -> error "unreachable: cloneId"
{-# INLINE cloneMeta #-}

alphaExpr ::
  (Reader Subst :> es, Reader ModuleName :> es, State Uniq :> es) =>
  Expr (Meta Type) ->
  Eff es (Expr (Meta Type))
alphaExpr (Atom x) = Atom <$> alphaAtom x
alphaExpr (Call f xs) = Call <$> alphaAtom f <*> traverse alphaAtom xs
alphaExpr (CallDirect f xs) = CallDirect <$> lookupId f <*> traverse alphaAtom xs
alphaExpr (RawCall n t xs) = RawCall n t <$> traverse alphaAtom xs
alphaExpr (Cast t x) = Cast t <$> alphaAtom x
alphaExpr (Let ds e) = do
  -- Avoid capturing variables
  env <- foldMapM (\(LocalDef n _ _) -> Map.singleton n . Var <$> cloneMeta n) ds
  local (env <>) $ Let <$> traverse alphaLocalDef ds <*> alphaExpr e
alphaExpr (Match e cs) = Match <$> alphaExpr e <*> traverse alphaCase cs
alphaExpr (Switch v cs e) = Switch <$> alphaAtom v <*> traverse (\(tag, e) -> (tag,) <$> alphaExpr e) cs <*> alphaExpr e
alphaExpr (SwitchUnboxed v cs e) = SwitchUnboxed <$> alphaAtom v <*> traverse (\(tag, e) -> (tag,) <$> alphaExpr e) cs <*> alphaExpr e
alphaExpr (Destruct v con xs e) = do
  -- Avoid capturing variables
  env <- foldMapM (\x -> Map.singleton x . Var <$> cloneMeta x) xs
  local (env <>) $ Destruct <$> alphaAtom v <*> pure con <*> traverse lookupId xs <*> alphaExpr e
alphaExpr (DestructRecord v kvs e) = do
  -- Avoid capturing variables
  let xs = Map.elems kvs
  env <- foldMapM (\x -> Map.singleton x . Var <$> cloneMeta x) xs
  local (env <>) $ DestructRecord <$> alphaAtom v <*> traverse lookupId kvs <*> alphaExpr e
alphaExpr (Assign x v e) = do
  v' <- alphaExpr v
  x' <- cloneMeta x
  local (Map.insert x (Var x')) $ Assign x' v' <$> alphaExpr e
alphaExpr (Error t) = pure $ Error t

alphaAtom :: (Reader Subst :> es) => Atom (Meta Type) -> Eff es (Atom (Meta Type))
alphaAtom (Var x) = lookupVar x
alphaAtom a@Unboxed {} = pure a

alphaLocalDef ::
  (Reader Subst :> es, Reader ModuleName :> es, State Uniq :> es) =>
  LocalDef (Meta Type) ->
  Eff es (LocalDef (Meta Type))
alphaLocalDef (LocalDef x t o) =
  -- Since `alphaExpr Let{}` avoids capturing variables, only `lookupId` should be applied here.
  LocalDef <$> lookupId x <*> pure t <*> alphaObj o

alphaObj ::
  (Reader Subst :> es, Reader ModuleName :> es, State Uniq :> es) =>
  Obj (Meta Type) ->
  Eff es (Obj (Meta Type))
alphaObj (Fun ps e) = do
  -- Avoid capturing variables
  ps' <- traverse cloneMeta ps
  local (Map.fromList (zip ps $ map Var ps') <>) $ Fun ps' <$> alphaExpr e
alphaObj o = traverseOf atom alphaAtom o

alphaCase ::
  (Reader Subst :> es, Reader ModuleName :> es, State Uniq :> es) =>
  Case (Meta Type) ->
  Eff es (Case (Meta Type))
alphaCase (Unpack c ps e) = do
  -- Avoid capturing variables
  ps' <- traverse cloneMeta ps
  local (Map.fromList (zip ps $ map Var ps') <>) $ Unpack c ps' <$> alphaExpr e
alphaCase (OpenRecord kps e) = do
  -- Avoid capturing variables
  let ps = Map.elems kps
  kps' <- traverse cloneMeta kps
  let ps' = Map.elems kps'
  local (Map.fromList (zip ps $ map Var ps') <>) $ OpenRecord kps' <$> alphaExpr e
alphaCase (Bind x t e) = do
  -- Avoid capturing variables
  x' <- cloneMeta x
  local (Map.insert x (Var x')) $ Bind x' t <$> alphaExpr e
alphaCase (Exact u e) = Exact u <$> alphaExpr e

equiv :: Expr (Meta Type) -> Expr (Meta Type) -> Maybe Subst
equiv e1 e2 = runPureEff $ runReader ?? mempty $ do
  isEquiv <- equivExpr @'[Reader Subst] e1 e2
  if isEquiv then Just <$> ask @Subst else pure Nothing

equivExpr :: (Reader Subst :> es) => Expr (Meta Type) -> Expr (Meta Type) -> Eff es Bool
equivExpr (Atom x) (Atom y) = equivAtom x y
equivExpr (Call f xs) (Call g ys) =
  (&&) <$> equivAtom f g <*> andM (zipWith equivAtom xs ys)
equivExpr (CallDirect f xs) (CallDirect g ys) = do
  subst <- ask
  let f' = fromMaybe (Var f) $ Map.lookup f subst
  let g' = fromMaybe (Var g) $ Map.lookup g subst
  rest <- andM $ zipWith equivAtom xs ys
  pure $ f' == g' && rest
equivExpr (RawCall n t xs) (RawCall m u ys) =
  ((n == m && t == u) &&) <$> andM (zipWith equivAtom xs ys)
equivExpr (Cast t x) (Cast u y) =
  (t == u &&) <$> equivAtom x y
equivExpr (Let ds e) (Let ds' e') = do
  subst <- mconcat <$> zipWithM equivLocalDef ds ds'
  if Map.null subst
    then pure False
    else do
      local (subst <>) $ equivExpr e e'
equivExpr (Match e cs) (Match e' cs') = do
  (&&) <$> equivExpr e e' <*> andM (zipWith equivCase cs cs')
equivExpr (Switch x cs e) (Switch y cs' e') = do
  andM [equivAtom x y, andM $ zipWith equivAlt cs cs', equivExpr e e']
  where
    equivAlt (tag, e) (tag', e') | tag == tag' = equivExpr e e'
    equivAlt _ _ = pure False
equivExpr (SwitchUnboxed x cs e) (SwitchUnboxed y cs' e') = do
  andM [equivAtom x y, andM $ zipWith equivAlt cs cs', equivExpr e e']
  where
    equivAlt (tag, e) (tag', e') | tag == tag' = equivExpr e e'
    equivAlt _ _ = pure False
equivExpr (Destruct x c ps e) (Destruct y c' ps' e') | c == c' = do
  (&&)
    <$> equivAtom x y
    <*> local
      (Map.fromList (zip ps $ map Var ps') <>)
      (equivExpr e e')
equivExpr (DestructRecord x kps e) (DestructRecord y kps' e') = do
  (&&)
    <$> equivAtom x y
    <*> local
      (Map.fromList (zip (Map.elems kps) (map Var $ Map.elems kps')) <>)
      (equivExpr e e')
equivExpr (Assign x e b) (Assign y e' b') =
  (&&)
    <$> equivExpr e e'
    <*> local (Map.insert x (Var y)) (equivExpr b b')
equivExpr (Error t) (Error u) = pure $ t == u
equivExpr _ _ = pure False

equivCase :: (Reader Subst :> es) => Case (Meta Type) -> Case (Meta Type) -> Eff es Bool
equivCase (Unpack c ps e) (Unpack c' ps' e') | c == c' = do
  local (Map.fromList (zip ps $ map Var ps') <>) $ equivExpr e e'
equivCase (OpenRecord kps e) (OpenRecord kps' e') = do
  local
    (Map.fromList (zip (Map.elems kps) (map Var $ Map.elems kps')) <>)
    $ equivExpr e e'
equivCase (Exact u e) (Exact u' e')
  | u == u' =
      equivExpr e e'
equivCase (Bind x t e) (Bind y u f) | t == u = do
  local (Map.insert x (Var y)) $ equivExpr e f
equivCase _ _ = pure False

equivLocalDef ::
  (Reader Subst :> es) =>
  LocalDef (Meta Type) ->
  LocalDef (Meta Type) ->
  Eff es (Map (Meta Type) (Atom (Meta Type)))
equivLocalDef (LocalDef x t o) (LocalDef y u p) =
  local (Map.insert x (Var y))
    $ ifM
      ((t == u &&) <$> equivObj o p)
      (pure $ Map.singleton x (Var y))
      (pure mempty)

equivObj :: (Reader Subst :> es) => Obj (Meta Type) -> Obj (Meta Type) -> Eff es Bool
equivObj (Fun ps e) (Fun ps' e') = do
  local (Map.fromList (zip ps $ map Var ps') <>) $ equivExpr e e'
equivObj (Pack t c xs) (Pack u d ys) =
  ((t == u && c == d) &&) <$> andM (zipWith equivAtom xs ys)
equivObj (Record kvs) (Record kvs') =
  andM $ Map.intersectionWith equivAtom kvs kvs'
equivObj _ _ = pure False

equivAtom :: (Reader Subst :> es) => Atom (Meta Type) -> Atom (Meta Type) -> Eff es Bool
equivAtom (Var x) (Var y) = do
  subst <- ask
  let x' = fromMaybe (Var x) $ Map.lookup x subst
  let y' = fromMaybe (Var y) $ Map.lookup y subst
  pure $ x' == y'
equivAtom x y = pure $ x == y
