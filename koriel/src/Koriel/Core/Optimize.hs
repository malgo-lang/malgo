{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Koriel.Core.Optimize
  ( optimize,
  )
where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Koriel.Core.Alpha
import Koriel.Core.Core
import Koriel.Core.Flat
import Koriel.Core.Type
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude

times :: (Monad m, Eq (t a), Foldable t) => Int -> (t a -> m (t a)) -> t a -> m (t a)
times n f e =
  if n <= 0
    then pure e
    else do
      e' <- f e
      if e == e' then pure e' else times (n - 1) f e'

optimize :: MonadUniq m => Int -> Exp (Id Type) -> m (Exp (Id Type))
optimize level expr =
  runReaderT
    ?? level
    $ flat
      <$> times
        10
        ( optVarBind
            >=> (flip runReaderT mempty . optPackInline)
            >=> removeUnusedLet
            >=> (flip evalStateT mempty . optCallInline)
            >=> optCast
            >=> pure
              . flat
        )
        expr

type CallInlineMap = Map (Id Type) ([Id Type], Exp (Id Type))

optCallInline ::
  (MonadState CallInlineMap f, MonadReader Int f, MonadUniq f) =>
  Exp (Id Type) ->
  f (Exp (Id Type))
optCallInline (Call (Var f) xs) = lookupCallInline f xs
optCallInline (Match v cs) =
  Match <$> optCallInline v <*> traverseOf (traversed . appCase) optCallInline cs
optCallInline (Let ds e) = do
  ds' <- traverseOf (traversed . _2 . appObj) optCallInline ds
  traverse_ checkInlineable ds'
  Let <$> traverseOf (traversed . _2 . appObj) optCallInline ds' <*> optCallInline e
optCallInline e = pure e

checkInlineable ::
  (MonadState CallInlineMap m, MonadReader Int m) =>
  (Id Type, Obj (Id Type)) ->
  m ()
checkInlineable (f, Fun ps v) = do
  level <- ask
  -- 変数の数がinlineSize以下ならインライン展開する
  when (length v <= level || f `notElem` freevars v) $ at f ?= (ps, v)
checkInlineable _ = pure ()

lookupCallInline ::
  (MonadUniq m, MonadState CallInlineMap m) =>
  Id Type ->
  [Atom (Id Type)] ->
  m (Exp (Id Type))
lookupCallInline f as = do
  f' <- gets (view (at f))
  case f' of
    Just (ps, v) -> alpha v (Map.fromList $ zip ps as)
    Nothing -> pure $ Call (Var f) as

type PackInlineMap = Map (Id Type) (Con, [Atom (Id Type)])

optPackInline :: MonadReader PackInlineMap m => Exp (Id Type) -> m (Exp (Id Type))
optPackInline (Match (Atom (Var v)) (Unpack con xs body :| [])) = do
  body' <- optPackInline body
  mPack <- asks $ view (at v)
  case mPack of
    Just (con', as) | con == con' -> pure $ build xs as body'
    _ -> pure $ Match (Atom $ Var v) $ Unpack con xs body' :| []
  where
    build (x : xs) (a : as) body = Match (Atom a) $ Bind x (build xs as body) :| []
    build _ _ body = body
optPackInline (Match v cs) =
  Match <$> optPackInline v <*> traverseOf (traversed . appCase) optPackInline cs
optPackInline (Let ds e) = do
  ds' <- traverseOf (traversed . _2 . appObj) optPackInline ds
  local (mconcat (map toPackInlineMap ds') <>) $ Let ds' <$> optPackInline e
  where
    toPackInlineMap (v, Pack _ con as) = mempty & at v ?~ (con, as)
    toPackInlineMap _ = mempty
optPackInline e = pure e

optVarBind :: (Eq a, Applicative f) => Exp a -> f (Exp a)
optVarBind (Match (Atom a) (Bind x e :| [])) = replaceOf atom (Var x) a <$> optVarBind e
optVarBind (Let ds e) = Let <$> traverseOf (traversed . _2 . appObj) optVarBind ds <*> optVarBind e
optVarBind (Match v cs) = Match <$> optVarBind v <*> traverseOf (traversed . appCase) optVarBind cs
optVarBind e = pure e

removeUnusedLet :: (Monad f, Ord a) => Exp a -> f (Exp a)
removeUnusedLet (Let ds e) = do
  ds' <- traverseOf (traversed . _2 . appObj) removeUnusedLet ds
  e' <- removeUnusedLet e
  let gamma = map (\(v, o) -> (v, Set.delete v $ freevars o)) ds'
  let ds'' = filter (\(v, _) -> reachable 100 gamma v $ freevars e') ds'
  if null ds'' then pure e' else pure $ Let ds'' e'
  where
    reachable limit gamma v fvs
      | limit <= 0 =
        undefined
      | v `elem` fvs =
        True
      | otherwise =
        let fvs' = fvs <> mconcat (mapMaybe (List.lookup ?? gamma) $ Set.toList fvs)
         in fvs /= fvs' && reachable (limit - 1 :: Int) gamma v fvs'
removeUnusedLet (Match v cs) =
  Match <$> removeUnusedLet v <*> traverseOf (traversed . appCase) removeUnusedLet cs
removeUnusedLet e = pure e

optCast :: MonadUniq f => Exp (Id Type) -> f (Exp (Id Type))
optCast e@(Cast (pts' :-> rt') f) = case typeOf f of
  pts :-> _
    | length pts' == length pts -> do
      f' <- newId "$cast_opt" (pts' :-> rt')
      ps' <- traverse (newId "$p") pts'
      v' <- runDef $ do
        ps <- zipWithM cast pts $ map (Atom . Var) ps'
        r <- bind (Call f ps)
        pure $ Cast rt' r
      pure (Let [(f', Fun ps' v')] (Atom $ Var f'))
    | otherwise -> bug Unreachable
  _ -> pure e
optCast (Match v cs) = Match <$> optCast v <*> traverseOf (traversed . appCase) optCast cs
optCast (Let ds e) = Let <$> traverseOf (traversed . _2 . appObj) optCast ds <*> optCast e
optCast e = pure e
