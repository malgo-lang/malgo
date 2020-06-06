{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Malgo.Core.Optimize
  ( Optimize,
  )
where

import Language.Malgo.Core.Flat
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
  -- trans e = pure e
  trans e = evalStateT ?? mempty $ optCallInline e >>= lift . trans @Flat >>= optVarBind

type InlineMap = IdMap CType ([Atom (Id CType)] -> Exp (Id CType))

replaceOf :: Eq a => ASetter' s a -> a -> a -> s -> s
replaceOf l x x' = over l (\v -> if v == x then x' else v)

optCallInline ::
  MonadState InlineMap f =>
  Exp (Id CType) ->
  f (Exp (Id CType))
optCallInline (Call (Var f) xs) = lookupInline f <*> pure xs
optCallInline (Match v cs) =
  Match <$> optCallInline v <*> traverse (appCase optCallInline) cs
optCallInline (Let ds e) = do
  ds' <- traverse (rtraverse (appObj optCallInline)) ds
  traverse_ checkInlineable ds'
  Let ds' <$> optCallInline e
  where
    checkInlineable (f, o@(Fun ps v)) =
      when (null $ freevars o)
        $ modify
        $ at f ?~ (\ps' -> go ps ps' v)
    checkInlineable _ = pure ()
    go [] [] v = v
    go (p : ps) (p' : ps') v = replaceOf atom (Var p) p' (go ps ps' v)
    go _ _ _ = bug Unreachable
optCallInline e = pure e

lookupInline ::
  MonadState InlineMap m =>
  Id CType ->
  m ([Atom (Id CType)] -> Exp (Id CType))
lookupInline f = do
  f' <- gets (view (at f))
  pure $ case f' of
    Just inline -> inline
    Nothing -> Call (Var f)

optVarBind :: (Eq a, Applicative f) => Exp a -> f (Exp a)
optVarBind (Match (Atom (Var x)) (Bind x' e :| [])) = replaceOf mapped x' x <$> optVarBind e
optVarBind (Let ds e) = Let <$> traverse (rtraverse (appObj optVarBind)) ds <*> optVarBind e
optVarBind (Match v cs) = Match <$> optVarBind v <*> traverse (appCase optVarBind) cs
optVarBind e = pure e
