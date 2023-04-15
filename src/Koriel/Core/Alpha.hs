{-# LANGUAGE TemplateHaskell #-}

-- | Alpha conversion
module Koriel.Core.Alpha
  ( alpha,
    AlphaEnv (..),
  )
where

import Control.Exception (assert)
import Control.Lens (makeFieldsNoPrefix, traverseOf)
import Data.HashMap.Strict qualified as HashMap
import Data.String.Conversions (convertString)
import Koriel.Core.Syntax
import Koriel.Core.Type
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude
import Numeric (showHex)

-- | Environment for alpha conversion
data AlphaEnv = AlphaEnv
  { uniqSupply :: UniqSupply,
    -- | Substitution
    subst :: HashMap (Id Type) (Atom (Id Type))
  }

makeFieldsNoPrefix ''AlphaEnv

-- | Alpha conversion
alpha :: MonadIO m => Expr (Id Type) -> AlphaEnv -> m (Expr (Id Type))
alpha = runAlpha . alphaExpr

runAlpha :: ReaderT AlphaEnv m a -> AlphaEnv -> m a
runAlpha = runReaderT

lookupVar :: MonadReader AlphaEnv m => Id Type -> m (Atom (Id Type))
lookupVar n = do
  env <- asks (.subst)
  pure $ HashMap.lookupDefault (Var n) n env

lookupId :: (MonadReader AlphaEnv m) => Id Type -> m (Id Type)
lookupId n = do
  n' <- lookupVar n
  case n' of
    Var i -> pure i
    _ -> error $ show n <> " must be bound to Var"

cloneId :: (MonadIO m, MonadReader AlphaEnv m) => Id a -> m (Id a)
cloneId Id {..} = do
  assert (sort == Internal || sort == Temporal) pass -- only Internal or Temporal id can be cloned
  uniq <- getUniq
  name <- pure $ name <> "_" <> convertString (showHex uniq "")
  pure Id {..}

alphaExpr :: (MonadReader AlphaEnv f, MonadIO f) => Expr (Id Type) -> f (Expr (Id Type))
alphaExpr (Atom x) = Atom <$> alphaAtom x
alphaExpr (Call f xs) = Call <$> alphaAtom f <*> traverse alphaAtom xs
alphaExpr (CallDirect f xs) = CallDirect <$> lookupId f <*> traverse alphaAtom xs
alphaExpr (RawCall n t xs) = RawCall n t <$> traverse alphaAtom xs
alphaExpr (BinOp op x y) = BinOp op <$> alphaAtom x <*> alphaAtom y
alphaExpr (Cast t x) = Cast t <$> alphaAtom x
alphaExpr (Let ds e) = do
  -- Avoid capturing variables
  env <- foldMapM (\(LocalDef n _ _) -> one . (n,) . Var <$> cloneId n) ds
  local (\e -> e {subst = env <> e.subst}) $ Let <$> traverse alphaLocalDef ds <*> alphaExpr e
alphaExpr (Match e cs) = Match <$> alphaExpr e <*> traverse alphaCase cs
alphaExpr (Switch v cs e) = Switch <$> alphaAtom v <*> traverse (\(tag, e) -> (tag,) <$> alphaExpr e) cs <*> alphaExpr e
alphaExpr (SwitchUnboxed v cs e) = SwitchUnboxed <$> alphaAtom v <*> traverse (\(tag, e) -> (tag,) <$> alphaExpr e) cs <*> alphaExpr e
alphaExpr (Destruct v con xs e) = do
  -- Avoid capturing variables
  env <- foldMapM (\x -> one . (x,) . Var <$> cloneId x) xs
  local (\e -> e {subst = env <> e.subst}) $ Destruct <$> alphaAtom v <*> pure con <*> traverse lookupId xs <*> alphaExpr e
alphaExpr (DestructRecord v kvs e) = do
  -- Avoid capturing variables
  let xs = HashMap.elems kvs
  env <- foldMapM (\x -> one . (x,) . Var <$> cloneId x) xs
  local (\e -> e {subst = env <> e.subst}) $ DestructRecord <$> alphaAtom v <*> traverse lookupId kvs <*> alphaExpr e
alphaExpr (Assign x v e) = do
  v' <- alphaExpr v
  x' <- cloneId x
  local (\e -> e {subst = one (x, Var x') <> e.subst}) $ Assign x' v' <$> alphaExpr e
alphaExpr (Error t) = pure $ Error t

alphaAtom :: (MonadReader AlphaEnv f) => Atom (Id Type) -> f (Atom (Id Type))
alphaAtom (Var x) = lookupVar x
alphaAtom a@Unboxed {} = pure a

alphaLocalDef :: (MonadReader AlphaEnv f, MonadIO f) => LocalDef (Id Type) -> f (LocalDef (Id Type))
alphaLocalDef (LocalDef x t o) =
  -- Since `alphaExpr Let{}` avoids capturing variables, only `lookupId` should be applied here.
  LocalDef <$> lookupId x <*> pure t <*> alphaObj o

alphaObj :: (MonadReader AlphaEnv m, MonadIO m) => Obj (Id Type) -> m (Obj (Id Type))
alphaObj (Fun ps e) = do
  -- Avoid capturing variables
  ps' <- traverse cloneId ps
  local (\e -> e {subst = HashMap.fromList (zip ps $ map Var ps') <> e.subst}) $ Fun ps' <$> alphaExpr e
-- local (\e -> e {subst = HashMap.filterWithKey (\k _ -> k `notElem` ps) e.subst}) $ Fun ps <$> alphaExpr e
alphaObj o = traverseOf atom alphaAtom o

alphaCase :: (MonadReader AlphaEnv m, MonadIO m) => Case (Id Type) -> m (Case (Id Type))
alphaCase (Unpack c ps e) = do
  -- Avoid capturing variables
  ps' <- traverse cloneId ps
  local (\e -> e {subst = HashMap.fromList (zip ps $ map Var ps') <> e.subst}) $ Unpack c ps' <$> alphaExpr e
-- local (\e -> e {subst = HashMap.filterWithKey (\k _ -> k `notElem` ps) e.subst}) $ Unpack c ps <$> alphaExpr e
alphaCase (OpenRecord kps e) = do
  -- Avoid capturing variables
  let ps = HashMap.elems kps
  kps' <- traverse cloneId kps
  let ps' = HashMap.elems kps'
  local (\e -> e {subst = HashMap.fromList (zip ps $ map Var ps') <> e.subst}) $ OpenRecord kps' <$> alphaExpr e
-- local (\e -> e {subst = HashMap.filterWithKey (\k _ -> k `notElem` ps) e.subst}) $ OpenRecord kps <$> alphaExpr e
alphaCase (Bind x t e) = do
  -- Avoid capturing variables
  x' <- cloneId x
  local (\e -> e {subst = HashMap.insert x (Var x') e.subst}) $ Bind x' t <$> alphaExpr e
-- local (\e -> e {subst = HashMap.delete x e.subst}) $ Bind x <$> alphaExpr e
alphaCase (Exact u e) = Exact u <$> alphaExpr e