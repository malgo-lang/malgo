{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Koriel.Core.Optimize
  ( optimizeProgram,
  )
where

import Control.Monad.Except
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

-- | Apply a monadic function n times.
times :: Monad m => Int -> (t -> m t) -> t -> m t
times 0 _ x = pure x
times n f x
  | n > 0 = times (n - 1) f =<< f x
  | otherwise = bug Unreachable

-- | 最適化を行う関数
--
-- * 自明な変数の付け替えの簡約 (optVarBind)
-- * 値コンストラクタを含む関数のインライン展開 (optPackInline, optCallInline)
-- * 不要なlet式の削除 (removeUnusedLet)
optimizeProgram ::
  MonadUniq m =>
  -- | インライン展開する関数のサイズ
  Int ->
  Program (Id Type) ->
  m (Program (Id Type))
optimizeProgram level prog@(Program fs) = runReaderT ?? level $ do
  state <- execStateT (traverse (checkInlineable . over _2 (uncurry Fun)) fs) mempty
  appProgram (optimizeExpr state) prog

optimizeExpr :: (MonadReader Int f, MonadUniq f) => CallInlineMap -> Exp (Id Type) -> f (Exp (Id Type))
optimizeExpr state = 10 `times` opt
  where
    opt =
      pure
        >=> optVarBind
        >=> (flip runReaderT mempty . optPackInline)
        >=> removeUnusedLet
        >=> (flip evalStateT state . optCallInline)
        -- >=> optCast
        >=> pure
          . flat

type CallInlineMap = Map (Id Type) ([Id Type], Exp (Id Type))

optCallInline ::
  (MonadState CallInlineMap f, MonadReader Int f, MonadUniq f) =>
  Exp (Id Type) ->
  f (Exp (Id Type))
optCallInline (Call (Var f) xs) = lookupCallInline f xs
-- CallDirectをインライン展開するとコードサイズが爆発する事がある。要調査
-- また、CallDirectをインライン展開して
-- `traverseOf appProgram (optimize (inlineSize opt))`を適用するとコードが壊れる。要調査
-- optCallInline (CallDirect f xs) = lookupCallInline f xs
optCallInline (Match v cs) =
  Match <$> optCallInline v <*> traverseOf (traversed . appCase) optCallInline cs
optCallInline (Let ds e) = do
  ds' <- traverseOf (traversed . _2 . appObj) optCallInline ds
  traverse_ checkInlineable ds'
  Let ds' <$> optCallInline e
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
  mPack <- view (at v)
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

removeUnusedLet :: (Monad f, Ord a) => Exp (Id a) -> f (Exp (Id a))
removeUnusedLet (Let ds e) = do
  ds' <- traverseOf (traversed . _2 . appObj) removeUnusedLet ds
  e' <- removeUnusedLet e
  -- 定義vから到達可能でかつvで定義されていない変数すべての集合のマップ
  let gamma = map (\(v, o) -> (v, Set.delete v $ freevars o)) ds'
  if any (\(v, _) -> reachable 100 gamma v $ freevars e') ds' then pure $ Let ds' e' else pure e'
  where
    reachable :: Ord a => Int -> [(Id a, Set (Id a))] -> Id a -> Set (Id a) -> Bool
    reachable limit gamma v fvs
      -- limit回試行してわからなければ安全側に倒してTrue
      | limit <= 0 = True
      | v ^. idIsTopLevel = True
      | v `elem` fvs = True
      | otherwise =
        -- fvsの要素fvについて、gamma[fv]をfvsに加える
        -- fvsに変化がなければ、vはどこからも参照されていない
        let fvs' = fvs <> mconcat (mapMaybe (List.lookup ?? gamma) $ Set.toList fvs)
         in fvs /= fvs' && reachable limit gamma v fvs'
removeUnusedLet (Match v cs) =
  Match <$> removeUnusedLet v <*> traverseOf (traversed . appCase) removeUnusedLet cs
removeUnusedLet e = pure e

-- 効果がはっきりしないので一旦コメントアウト
-- TODO: ベンチマーク
-- optCast :: MonadUniq f => Exp (Id Type) -> f (Exp (Id Type))
-- optCast e@(Cast (pts' :-> rt') f) = case typeOf f of
--   pts :-> _
--     | length pts' == length pts -> do
--       f' <- newLocalId "$cast_opt" (pts' :-> rt')
--       ps' <- traverse (newLocalId "$p") pts'
--       v' <- runDef do
--         ps <- zipWithM cast pts $ map (Atom . Var) ps'
--         r <- bind (Call f ps)
--         pure $ Cast rt' r
--       pure (Let [(f', Fun ps' v')] (Atom $ Var f'))
--     | otherwise -> bug Unreachable
--   _ -> pure e
-- optCast (Match v cs) = Match <$> optCast v <*> traverseOf (traversed . appCase) optCast cs
-- optCast (Let ds e) = Let <$> traverseOf (traversed . _2 . appObj) optCast ds <*> optCast e
-- optCast e = pure e
