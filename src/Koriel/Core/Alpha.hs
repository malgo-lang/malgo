{-# LANGUAGE TemplateHaskell #-}

module Koriel.Core.Alpha
  ( alpha,
    AlphaEnv (..),
  )
where

-- α変換

import qualified Data.HashMap.Strict as HashMap
import Koriel.Core.Syntax
import Koriel.Core.Type
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude

data AlphaEnv = AlphaEnv {_alphaUniqSupply :: UniqSupply, _alphaMap :: HashMap (Id Type) (Atom (Id Type))}

makeLenses ''AlphaEnv

instance HasUniqSupply AlphaEnv where
  uniqSupply = alphaUniqSupply

alpha :: MonadIO m => Exp (Id Type) -> AlphaEnv -> m (Exp (Id Type))
alpha = runAlpha . alphaExp

runAlpha :: ReaderT AlphaEnv m a -> AlphaEnv -> m a
runAlpha = runReaderT

lookupVar :: MonadReader AlphaEnv m => Id Type -> m (Atom (Id Type))
lookupVar n = do
  env <- view alphaMap
  case HashMap.lookup n env of
    Just n' -> pure n'
    Nothing -> pure (Var n)

lookupId :: (MonadReader AlphaEnv m) => Id Type -> m (Id Type)
lookupId n = do
  n' <- lookupVar n
  case n' of
    Var i -> pure i
    _ -> bug $ Unreachable $ tshow n <> " must be bound to Var"

alphaExp :: (MonadReader AlphaEnv f, MonadIO f) => Exp (Id Type) -> f (Exp (Id Type))
alphaExp (CallDirect f xs) = CallDirect <$> lookupId f <*> traverse alphaAtom xs
alphaExp (Let ds e) = do
  env <- foldMapA ?? ds $ \(LocalDef n _) -> HashMap.singleton n . Var <$> cloneId n
  local (over alphaMap (env <>)) $ Let <$> traverse alphaLocalDef ds <*> alphaExp e
alphaExp (Match e cs) = Match <$> alphaExp e <*> traverse alphaCase cs
alphaExp e = traverseOf atom alphaAtom e

alphaAtom :: (MonadReader AlphaEnv f) => Atom (Id Type) -> f (Atom (Id Type))
alphaAtom (Var x) = lookupVar x
alphaAtom a@Unboxed {} = pure a

alphaLocalDef :: (MonadReader AlphaEnv f, MonadIO f) => LocalDef (Id Type) -> f (LocalDef (Id Type))
alphaLocalDef (LocalDef x o) = LocalDef <$> lookupId x <*> alphaObj o

alphaObj :: (MonadReader AlphaEnv m, MonadIO m) => Obj (Id Type) -> m (Obj (Id Type))
alphaObj (Fun ps e) = do
  ps' <- traverse cloneId ps
  local (over alphaMap (HashMap.fromList (zip ps $ map Var ps') <>)) $ Fun ps' <$> alphaExp e
alphaObj o = traverseOf atom alphaAtom o

alphaCase :: (MonadReader AlphaEnv m, MonadIO m) => Case (Id Type) -> m (Case (Id Type))
alphaCase (Unpack c ps e) = do
  ps' <- traverse cloneId ps
  local (over alphaMap (HashMap.fromList (zip ps $ map Var ps') <>)) $ Unpack c ps' <$> alphaExp e
alphaCase (Bind x e) = do
  x' <- cloneId x
  local (over alphaMap (HashMap.insert x $ Var x')) $ Bind x' <$> alphaExp e
alphaCase (Switch u e) = Switch u <$> alphaExp e