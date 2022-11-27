{-# LANGUAGE TemplateHaskell #-}

-- | Alpha conversion
module Koriel.Core.Alpha
  ( alpha,
    AlphaEnv (..),
  )
where

import Control.Lens (makeFieldsNoPrefix, traverseOf)
import Data.HashMap.Strict qualified as HashMap
import Koriel.Core.Syntax
import Koriel.Core.Type
import Koriel.Id
import Koriel.Lens
import Koriel.MonadUniq
import Koriel.Prelude

data AlphaEnv = AlphaEnv {_uniqSupply :: UniqSupply, subst :: HashMap (Id Type) (Atom (Id Type))}

makeFieldsNoPrefix ''AlphaEnv

alpha :: MonadIO m => Exp (Id Type) -> AlphaEnv -> m (Exp (Id Type))
alpha = runAlpha . alphaExp

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

alphaExp :: (MonadReader AlphaEnv f, MonadIO f) => Exp (Id Type) -> f (Exp (Id Type))
alphaExp (CallDirect f xs) = CallDirect <$> lookupId f <*> traverse alphaAtom xs
alphaExp (Let ds e) = do
  -- Avoid capturing variables
  env <- foldMapM (\(LocalDef n _) -> one . (n,) . Var <$> cloneId n) ds
  local (\e -> e {subst = env <> e.subst}) $ Let <$> traverse alphaLocalDef ds <*> alphaExp e
-- このコードでも動くはずだけど、segfaultする。
-- let binds = map (._localDefVar) ds
-- e' <- local (\e -> e {subst = HashMap.filterWithKey (\k _ -> k `notElem` binds) e.subst}) $ do
--   Let <$> traverse alphaLocalDef ds <*> alphaExp e
alphaExp (Match e cs) = Match <$> alphaExp e <*> traverse alphaCase cs
alphaExp e = traverseOf atom alphaAtom e

alphaAtom :: (MonadReader AlphaEnv f) => Atom (Id Type) -> f (Atom (Id Type))
alphaAtom (Var x) = lookupVar x
alphaAtom a@Unboxed {} = pure a

alphaLocalDef :: (MonadReader AlphaEnv f, MonadIO f) => LocalDef (Id Type) -> f (LocalDef (Id Type))
alphaLocalDef (LocalDef x o) =
  -- Since `alphaExp Let{}` avoids capturing variables, only `lookupId` should be applied here.
  LocalDef <$> lookupId x <*> alphaObj o

alphaObj :: (MonadReader AlphaEnv m, MonadIO m) => Obj (Id Type) -> m (Obj (Id Type))
alphaObj (Fun ps e) = do
  -- Avoid capturing variables
  ps' <- traverse cloneId ps
  local (\e -> e {subst = HashMap.fromList (zip ps $ map Var ps') <> e.subst}) $ Fun ps' <$> alphaExp e
-- local (\e -> e {subst = HashMap.filterWithKey (\k _ -> k `notElem` ps) e.subst}) $ Fun ps <$> alphaExp e
alphaObj o = traverseOf atom alphaAtom o

alphaCase :: (MonadReader AlphaEnv m, MonadIO m) => Case (Id Type) -> m (Case (Id Type))
alphaCase (Unpack c ps e) = do
  -- Avoid capturing variables
  ps' <- traverse cloneId ps
  local (\e -> e {subst = HashMap.fromList (zip ps $ map Var ps') <> e.subst}) $ Unpack c ps' <$> alphaExp e
-- local (\e -> e {subst = HashMap.filterWithKey (\k _ -> k `notElem` ps) e.subst}) $ Unpack c ps <$> alphaExp e
alphaCase (OpenRecord kps e) = do
  -- Avoid capturing variables
  let ps = HashMap.elems kps
  kps' <- traverse cloneId kps
  let ps' = HashMap.elems kps'
  local (\e -> e {subst = HashMap.fromList (zip ps $ map Var ps') <> e.subst}) $ OpenRecord kps' <$> alphaExp e
-- local (\e -> e {subst = HashMap.filterWithKey (\k _ -> k `notElem` ps) e.subst}) $ OpenRecord kps <$> alphaExp e
alphaCase (Bind x e) = do
  -- Avoid capturing variables
  x' <- cloneId x
  local (\e -> e {subst = HashMap.insert x (Var x') e.subst}) $ Bind x' <$> alphaExp e
-- local (\e -> e {subst = HashMap.delete x e.subst}) $ Bind x <$> alphaExp e
alphaCase (Switch u e) = Switch u <$> alphaExp e