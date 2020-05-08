{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Malgo.Core.Optimize
  ( Optimize,
  )
where

import Language.Malgo.IR.Core
import Language.Malgo.Id
import Language.Malgo.Monad
import Language.Malgo.Pass
import Language.Malgo.Prelude
import Language.Malgo.TypeRep.CType

data Optimize

instance Pass Optimize (Exp (Id CType)) (Exp (Id CType)) where
  passName = "optimize"
  isDump = dumpDesugar
  trans e = evalStateT ?? mempty $ optCallInline e >>= optVarBind

replace :: (Functor f, Eq a) => a -> a -> f a -> f a
replace x x' = fmap (\v -> if v == x then x' else v)

optCallInline ::
  MonadState
    (IdMap CType ([Id CType] -> Exp (Id CType)))
    f =>
  Exp (Id CType) ->
  f (Exp (Id CType))
optCallInline (Call f xs) | all isVar xs = do
  f' <- lookupInline f
  pure $ f' (map (\case Var v' -> v'; Unboxed {} -> bug Unreachable) xs)
  where
    isVar Var {} = True
    isVar _ = False
optCallInline (Match v cs) =
  Match <$> optCallInline v <*> traverse (appCase optCallInline) cs
optCallInline (Let ds e) = do
  ds' <- traverse (rtraverse (appObj optCallInline)) ds
  traverse_ ?? ds' $ \case
    (f, o@(Fun ps v)) ->
      when (null $ freevars o)
        $ modify
        $ at f ?~ (\ps' -> go ps ps' v)
    _ -> pure ()
  Let ds' <$> optCallInline e
  where
    go [] [] v = v
    go (p : ps) (p' : ps') v = replace p p' (go ps ps' v)
    go _ _ _ = bug Unreachable
optCallInline e = pure e

lookupInline ::
  ( MonadState (IdMap CType ([Id CType] -> Exp (Id CType))) m
  ) =>
  Id CType ->
  m ([Id CType] -> Exp (Id CType))
lookupInline f = do
  f' <- gets (view (at f))
  pure $ case f' of
    Just inline -> inline :: [Id CType] -> Exp (Id CType)
    Nothing -> Call f . map Var

optVarBind :: (Eq a, Applicative f) => Exp a -> f (Exp a)
optVarBind (Match (Atom (Var x)) (Bind x' e :| [])) = replace x' x <$> optVarBind e
optVarBind (Let ds e) = Let <$> traverse (rtraverse (appObj optVarBind)) ds <*> optVarBind e
optVarBind (Match v cs) = Match <$> optVarBind v <*> traverse (appCase optVarBind) cs
optVarBind e = pure e

appObj ::
  Applicative f =>
  (Exp a -> f (Exp a)) ->
  Obj a ->
  f (Obj a)
appObj f (Fun ps e) = Fun ps <$> f e
appObj _ o = pure o

appCase ::
  Functor f =>
  (Exp a -> f (Exp a)) ->
  Case a ->
  f (Case a)
appCase f (Unpack con ps e) = Unpack con ps <$> f e
appCase f (Bind x e) = Bind x <$> f e
