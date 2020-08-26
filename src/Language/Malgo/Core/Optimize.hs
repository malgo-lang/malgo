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
  trans expr = do
    opt <- asks maOption
    if noOptimize opt
      then pure expr
      else
        trans @Flat
          =<< times
            10
            ( optVarBind
                >=> (flip runReaderT mempty . optPackInline)
                >=> removeUnusedLet
                >=> (flip evalStateT mempty . optCallInline)
            )
            expr
    where
      times :: (Monad m, Eq (t a), Foldable t) => Int -> (t a -> m (t a)) -> t a -> m (t a)
      times n f e =
        if n <= 0
          then pure e
          else do
            e' <- f e
            if e == e'
              then pure e'
              else times (n - 1) f e'

type CallInlineMap = IdMap CType ([Atom (Id CType)] -> Exp (Id CType))

optCallInline ::
  (MonadState CallInlineMap f, MonadMalgo f) =>
  Exp (Id CType) ->
  f (Exp (Id CType))
optCallInline (Call (Var f) xs) = lookupCallInline f <*> pure xs
optCallInline (Match v cs) =
  Match <$> optCallInline v <*> traverse (appCase optCallInline) cs
optCallInline (Let ds e) = do
  ds' <- traverse (rtraverse (appObj optCallInline)) ds
  traverse_ checkInlineable ds'
  Let ds' <$> optCallInline e
  where
    checkInlineable (f, Fun ps v) = do
      opt <- getOpt
      -- 変数の数がinlineSize以下ならインライン展開する
      when (length v <= inlineSize opt) $
        modify $ at f ?~ makeTemplate ps v
    checkInlineable _ = pure ()
    -- FunをHaskell上の関数に変換する
    makeTemplate [] v [] = v
    makeTemplate (p : ps) v (p' : ps') = replaceOf atom (Var p) p' (makeTemplate ps v ps')
    makeTemplate _ _ _ = bug Unreachable
optCallInline e = pure e

lookupCallInline ::
  MonadState CallInlineMap m =>
  Id CType ->
  m ([Atom (Id CType)] -> Exp (Id CType))
lookupCallInline f = do
  f' <- gets (view (at f))
  pure $ case f' of
    Just inline -> inline
    Nothing -> Call (Var f)

type PackInlineMap = IdMap CType (Con, [Atom (Id CType)])

optPackInline :: MonadReader PackInlineMap m => Exp (Id CType) -> m (Exp (Id CType))
optPackInline (Match (Atom (Var v)) (Unpack con xs body :| [])) = do
  body' <- optPackInline body
  mPack <- view (at v)
  case mPack of
    Just (con', as) | con == con' -> pure $ build xs as body'
    _ -> pure $ Match (Atom $ Var v) $ Unpack con xs body' :| []
  where
    build [] [] body = body
    build (x : xs) (a : as) body = Match (Atom a) $ Bind x (build xs as body) :| []
optPackInline (Match v cs) = Match <$> optPackInline v <*> traverse (appCase optPackInline) cs
optPackInline (Let ds e) = do
  ds' <- traverse (rtraverse (appObj optPackInline)) ds
  local (mconcat (map toPackInlineMap ds') <>) $ Let ds' <$> optPackInline e
  where
    toPackInlineMap (v, Pack _ con as) = mempty & at v ?~ (con, as)
    toPackInlineMap _ = mempty
optPackInline e = pure e

optVarBind :: (Eq a, Applicative f) => Exp a -> f (Exp a)
optVarBind (Match (Atom a) (Bind x e :| [])) = replaceOf atom (Var x) a <$> optVarBind e
optVarBind (Let ds e) = Let <$> traverse (rtraverse (appObj optVarBind)) ds <*> optVarBind e
optVarBind (Match v cs) = Match <$> optVarBind v <*> traverse (appCase optVarBind) cs
optVarBind e = pure e

removeUnusedLet :: (Monad f, Ord a) => Exp a -> f (Exp a)
removeUnusedLet (Let ds e) = do
  ds' <- traverse (rtraverse (appObj removeUnusedLet)) ds
  let fvs = freevars e <> mconcat (map (freevars . snd) ds')
  let ds'' = filter (\(v, _) -> v `elem` fvs) ds'
  if null ds''
    then removeUnusedLet e
    else Let ds'' <$> removeUnusedLet e
removeUnusedLet (Match v cs) = Match <$> removeUnusedLet v <*> traverse (appCase removeUnusedLet) cs
removeUnusedLet e = pure e