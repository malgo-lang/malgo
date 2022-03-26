module Koriel.Core.Alpha
  ( alpha,
    AlphaEnv (..),
  )
where

-- α変換

import Control.Lens (Lens', lens, over, traverseOf, view)
import qualified Data.HashMap.Strict as HashMap
import Koriel.Core.Syntax
import Koriel.Core.Type
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude

data AlphaEnv = AlphaEnv {_alphaUniqSupply :: UniqSupply, _alphaMap :: HashMap (Id Type) (Atom (Id Type))}

alphaUniqSupply :: Lens' AlphaEnv UniqSupply
alphaUniqSupply = lens _alphaUniqSupply (\a x -> a {_alphaUniqSupply = x})

alphaMap :: Lens' AlphaEnv (HashMap (Id Type) (Atom (Id Type)))
alphaMap = lens _alphaMap (\a x -> a {_alphaMap = x})

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
    _ -> error $ show n <> " must be bound to Var"

alphaExp :: (MonadReader AlphaEnv f, MonadIO f) => Exp (Id Type) -> f (Exp (Id Type))
alphaExp (CallDirect f xs) = CallDirect <$> lookupId f <*> traverse alphaAtom xs
alphaExp (Let ds e) = do
  env <- foldMapM ?? ds $ \(LocalDef n _) -> one . (n,) . Var <$> cloneId n
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
alphaCase (Case (Unpack c ps) e) = do
  ps' <- traverse cloneId ps
  local (over alphaMap (HashMap.fromList (zip ps $ map Var ps') <>)) $ Case (Unpack c ps') <$> alphaExp e
alphaCase (Case (Switch u) e) = Case (Switch u) <$> alphaExp e
alphaCase (Case (Bind x) e) = do
  x' <- cloneId x
  local (over alphaMap (HashMap.insert x $ Var x')) $ Case (Bind x') <$> alphaExp e