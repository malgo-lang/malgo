{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Core.Eval where

import qualified Data.Map as Map
import Language.Malgo.IR.Core
import Language.Malgo.Id
import Language.Malgo.Monad
import Language.Malgo.Prelude

data Cont a
  = MatchC [(Text, [a], Exp a)] (a, Exp a)
  | ApplyC [Atom a]
  deriving stock (Show)

data Obj a
  = FunO [a] (Exp a)
  | PapO a [Atom a]
  | PackO Text Int [Atom a]

-- step :: (Eq a, Ord a, MonadUniq m) => (Exp a, [Cont a], Map a (Obj a)) -> m (Exp a, [Cont a], Map a (Obj a))
-- step (Let x v e, s, h) = do
--   v' <- eval v h
--   x' <- newId (idMeta x) (idName x)
--   pure (subst x x' e, s, h & at x' ?~ v')
-- step (Match (Atom (Var v)) as (x, d), s, h) =
--   case h ^. at v of
--     Just (PackO tag n vs) -> go tag vs as
--     Nothing -> pure (subst x v d, s, h)
--   where
--     go tag0 vs ((tag1, ps, e):xs)
--       | tag0 == tag1 = pure (go' ps vs e, s, h)
--     go' [] [] e = e
--     go' (Var x:xs) (Var y:ys) e = go' xs ys (subst)

-- eval :: (Eq a, Ord a, MonadUniq m) => Exp a -> Map a (Obj a) -> m (Obj a)
-- eval (Atom (Var a)) = do
--   stack <- ask
--   heap <- get
--   case stack ^. at a of
--     (Just (Var a')) -> case heap ^. at a' of
--       Just
-- case (stack ^. at a, heap ^. at a) of
--   (Just (Var a'), _) -> pure v
--   (_, Just v) -> pure v
--   _           -> error "unbound variable"
-- eval (Atom (Lit _)) = error "unboxed value"
-- eval (Call (Known n) f xs) | n == length xs = do
--   heap <- get
--   case heap ^. at f of
--     Just (FunO ps e) -> local (Map.fromList (zip ps xs) <>) $ eval e

subst :: (Functor f, Eq a) => a -> a -> f a -> f a
subst a b f = fmap (\x -> if a == x then b else x) f

subst' :: (Functor f, Eq a) => [a] -> [a] -> f a -> f a
subst' as bs f = appEndo ?? f $ mconcat $ zipWith (\a b -> Endo $ subst a b) as bs
