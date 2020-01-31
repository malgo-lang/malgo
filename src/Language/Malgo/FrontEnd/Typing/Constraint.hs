{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
module Language.Malgo.FrontEnd.Typing.Constraint
  ( Constraint(..)
  , solve
  , UnifyError(..)
  , inst1
  , instantiate
  )
where

import           Language.Malgo.Monad
import           Language.Malgo.FrontEnd.Typing.Subst
import           Language.Malgo.TypeRep.Type
import           Language.Malgo.Prelude
import           Control.Monad.Error.Class

data Constraint = Type :~ Type
  deriving stock (Eq, Show)

instance Substitutable Constraint where
  apply s (t1 :~ t2) = apply s t1 :~ apply s t2
  ftv (t1 :~ t2) = ftv t1 <> ftv t2

data UnifyError = MismatchConstructor TyCon TyCon
                | MismatchLength [Type] [Type]
                | InfinitType TyVar Type
                | MismatchLevel Type Type
  deriving stock Show

solve :: MonadMalgo m => [Constraint] -> m (Either UnifyError Subst)
solve cs = runExceptT $ solver (mempty, cs)

solver :: MonadError UnifyError m => (Subst, [Constraint]) -> m Subst
solver (su, []             ) = pure su
solver (su, (t1 :~ t2) : cs) = do
  su1 <- unify t1 t2
  solver (su1 <> su, apply su1 cs)

inst1 :: MonadMalgo m => Type -> m Type
inst1 (TyForall vs ty) = do
  ts <- mapM (\_ -> TyMeta <$> newUniq) vs
  pure $ apply (Subst $ fromList $ zip vs ts) ty
inst1 t = pure t

instantiate :: MonadMalgo f => Type -> f Type
instantiate (TyMeta a      ) = pure $ TyMeta a
instantiate (TyApp    c  ts) = TyApp c <$> mapM instantiate ts
instantiate (TyForall vs t ) = do
  ts <- mapM (\_ -> TyMeta <$> newUniq) vs
  t' <- instantiate t
  pure $ apply (Subst $ fromList $ zip vs ts) t'

unify :: MonadError UnifyError m => Type -> Type -> m Subst
unify (TyMeta a) t          = bind a t
unify t          (TyMeta a) = bind a t
unify (TyApp c1 ts1) (TyApp c2 ts2) | c1 == c2  = unifyMany ts1 ts2
                                    | otherwise = throwError $ MismatchConstructor c1 c2
 where
  unifyMany []       []       = pure mempty
  unifyMany (x : xs) (y : ys) = do
    s1 <- unify x y
    s2 <- unifyMany (apply s1 xs) (apply s1 ys)
    pure $ s2 <> s1
  unifyMany _ _ = throwError $ MismatchLength ts1 ts2
unify (TyForall ts1 t1) (TyForall ts2 t2) | length ts1 == length ts2 = do
  let subst = Subst $ fromList $ zipWith (\v1 v2 -> (v1, TyMeta v2)) ts1 ts2
  unify (apply subst t1) (apply subst t2)
unify t1 t2 = throwError $ MismatchLevel t1 t2

bind :: MonadError UnifyError m => TyVar -> Type -> m Subst
bind a t | t == TyMeta a   = pure mempty
         | occursCheck a t = throwError $ InfinitType a t
         | otherwise       = pure $ Subst (one (a, t))

occursCheck :: Substitutable a => TyVar -> a -> Bool
occursCheck a t = a `member` ftv t
