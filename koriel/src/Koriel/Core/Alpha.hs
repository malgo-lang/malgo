{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Koriel.Core.Alpha
  ( alpha,
  )
where

-- α変換

import qualified Data.HashMap.Strict as HashMap
import Koriel.Core.Syntax
import Koriel.Core.Type
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude

alpha :: MonadUniq m => Exp (Id Type) -> AlphaEnv -> m (Exp (Id Type))
alpha = runAlpha . alphaExp

runAlpha :: ReaderT AlphaEnv m a -> AlphaEnv -> m a
runAlpha = runReaderT

type AlphaEnv = HashMap (Id Type) (Atom (Id Type))

cloneId :: MonadUniq f => Id a -> f (Id a)
cloneId n = newLocalId (n ^. idName) (n ^. idMeta)

lookupVar :: MonadReader AlphaEnv m => Id Type -> m (Atom (Id Type))
lookupVar n = do
  env <- ask
  case HashMap.lookup n env of
    Just n' -> pure n'
    Nothing -> pure (Var n)

lookupId :: (MonadReader AlphaEnv m) => Id Type -> m (Id Type)
lookupId n = do
  n' <- lookupVar n
  case n' of
    Var i -> pure i
    _ -> bug Unreachable

alphaExp :: (MonadReader AlphaEnv f, MonadUniq f) => Exp (Id Type) -> f (Exp (Id Type))
alphaExp (CallDirect f xs) = CallDirect <$> lookupId f <*> traverse alphaAtom xs
alphaExp (Let ds e) = do
  env <- foldMapA ?? ds $ \(LocalDef n _) -> HashMap.singleton n . Var <$> cloneId n
  local (env <>) $ Let <$> traverse alphaLocalDef ds <*> alphaExp e
alphaExp (Match e cs) = Match <$> alphaExp e <*> traverse alphaCase cs
alphaExp e = traverseOf atom alphaAtom e

alphaAtom :: (MonadReader AlphaEnv f) => Atom (Id Type) -> f (Atom (Id Type))
alphaAtom (Var x) = lookupVar x
alphaAtom a@Unboxed {} = pure a

alphaLocalDef :: (MonadReader AlphaEnv f, MonadUniq f) => LocalDef (Id Type) -> f (LocalDef (Id Type))
alphaLocalDef (LocalDef x o) = LocalDef <$> lookupId x <*> alphaObj o

alphaObj :: (MonadUniq m, MonadReader AlphaEnv m) => Obj (Id Type) -> m (Obj (Id Type))
alphaObj (Fun ps e) = do
  ps' <- traverse cloneId ps
  local (HashMap.fromList (zip ps $ map Var ps') <>) $ Fun ps' <$> alphaExp e
alphaObj o = traverseOf atom alphaAtom o

alphaCase :: (MonadUniq m, MonadReader AlphaEnv m) => Case (Id Type) -> m (Case (Id Type))
alphaCase (Unpack c ps e) = do
  ps' <- traverse cloneId ps
  local (HashMap.fromList (zip ps $ map Var ps') <>) $ Unpack c ps' <$> alphaExp e
alphaCase (Bind x e) = do
  x' <- cloneId x
  local (HashMap.insert x $ Var x') $ Bind x' <$> alphaExp e
alphaCase (Switch u e) = Switch u <$> alphaExp e