-- | Alpha conversion
module Malgo.Core.Alpha
  ( alpha,
    Subst,
  )
where

import Control.Exception (assert)
import Control.Lens (traverseOf)
import Data.Map.Strict qualified as Map
import Effectful (Eff, (:>))
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