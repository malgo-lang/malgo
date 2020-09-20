{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Core.Alpha (alphaProgram, alphaExp, runAlpha) where

-- α変換

import qualified Data.Map as Map
import Language.Malgo.IR.Core
import Language.Malgo.Id
import Language.Malgo.Monad (MonadUniq)
import Language.Malgo.Prelude
import Language.Malgo.TypeRep.CType

runAlpha :: ReaderT AlphaEnv m a -> AlphaEnv -> m a
runAlpha m env = runReaderT m env

type AlphaEnv = Map (Id CType) (Atom (Id CType))

cloneId :: MonadUniq f => Id a -> f (Id a)
cloneId n = newId (n ^. idMeta) (n ^. idName)

lookupVar :: (HasCallStack, MonadReader AlphaEnv m) => Id CType -> m (Atom (Id CType))
lookupVar n = do
  env <- ask
  case Map.lookup n env of
    Just n' -> pure n'
    Nothing -> pure (Var n)

lookupId :: MonadReader AlphaEnv m => Id CType -> m (Id CType)
lookupId n = do
  n' <- lookupVar n
  case n' of
    Var i -> pure i
    _ -> bug Unreachable

alphaProgram :: (MonadUniq f, MonadReader AlphaEnv f) => Program (Id CType) -> f (Program (Id CType))
alphaProgram (Program {topFuncs, mainExp}) = do
  newFuncIds <- foldMapA ?? topFuncs $ \(n, _) -> Map.singleton n <$> (Var <$> cloneId n)
  local (newFuncIds <>) $ do
    topFuncs' <- traverse alphaFunc topFuncs
    mainExp' <- alphaExp mainExp
    pure $ Program {topFuncs = topFuncs', mainExp = mainExp'}

alphaFunc :: (MonadUniq m, MonadReader AlphaEnv m) => (Id CType, ([Id CType], Exp (Id CType))) -> m (Id CType, ([Id CType], Exp (Id CType)))
alphaFunc (f, (ps, e)) = do
  ps' <- traverse cloneId ps
  local (Map.fromList (zip ps (map Var ps')) <>) $ do
    e' <- alphaExp e
    f' <- lookupId f
    pure (f', (ps', e'))

alphaExp :: (MonadReader AlphaEnv f, MonadUniq f) => Exp (Id CType) -> f (Exp (Id CType))
alphaExp (Atom a) = Atom <$> alphaAtom a
alphaExp (Call f xs) = Call <$> alphaAtom f <*> traverse alphaAtom xs
alphaExp (CallDirect f xs) = CallDirect <$> lookupId f <*> traverse alphaAtom xs
alphaExp (PrimCall f t xs) = PrimCall f t <$> traverse alphaAtom xs
alphaExp (BinOp op e1 e2) = BinOp op <$> alphaAtom e1 <*> alphaAtom e2
alphaExp (ArrayRead arr idx) = ArrayRead <$> alphaAtom arr <*> alphaAtom idx
alphaExp (ArrayWrite arr idx val) = ArrayWrite <$> alphaAtom arr <*> alphaAtom idx <*> alphaAtom val
alphaExp (Cast t e) = Cast t <$> alphaAtom e
alphaExp (Let ds e) = do
  env <- foldMapA ?? ds $ \(n, _) -> Map.singleton n . Var <$> cloneId n
  local (env <>) $
    Let <$> traverse (bitraverse lookupId alphaObj) ds <*> alphaExp e
alphaExp (Match e cs) = do
  Match <$> alphaExp e <*> traverse alphaCase cs
alphaExp e@Error {} = pure e

alphaAtom :: (MonadReader AlphaEnv f) => Atom (Id CType) -> f (Atom (Id CType))
alphaAtom (Var x) = lookupVar x
alphaAtom a@Unboxed {} = pure a

alphaObj :: (MonadUniq m, MonadReader AlphaEnv m) => Obj (Id CType) -> m (Obj (Id CType))
alphaObj (Fun ps e) = do
  ps' <- traverse cloneId ps
  local (Map.fromList (zip ps $ map Var ps') <>) $
    Fun ps' <$> alphaExp e
alphaObj (Pack t c xs) =
  Pack t c <$> traverse alphaAtom xs
alphaObj (Array val size) = Array <$> alphaAtom val <*> alphaAtom size

alphaCase :: (MonadUniq m, MonadReader AlphaEnv m) => Case (Id CType) -> m (Case (Id CType))
alphaCase (Unpack c ps e) = do
  ps' <- traverse cloneId ps
  local (Map.fromList (zip ps $ map Var ps') <>) $
    Unpack c ps' <$> alphaExp e
alphaCase (Bind x e) = do
  x' <- cloneId x
  local (Map.insert x $ Var x') $
    Bind x' <$> alphaExp e
