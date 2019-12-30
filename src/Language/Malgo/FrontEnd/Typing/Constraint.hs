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

data Constraint = Type :~ Type
  deriving (Eq, Show)

instance Substitutable Constraint where
  apply s (t1 :~ t2) = apply s t1 :~ apply s t2
  ftv (t1 :~ t2) = ftv t1 <> ftv t2

data UnifyError = MismatchConstructor TyCon TyCon
                | MismatchLength [Type] [Type]
                | InfinitType TyVar Type
                | MismatchLevel Type Type
  deriving Show

solve :: MonadMalgo m => [Constraint] -> m (Either UnifyError Subst)
solve cs = runExceptT $ solver (mempty, cs)

solver :: MonadMalgo m => (Subst, [Constraint]) -> ExceptT UnifyError m Subst
solver (su, []             ) = return su
solver (su, (t1 :~ t2) : cs) = do
  su1 <- unify t1 t2
  solver (su1 <> su, apply su1 cs)

inst1 :: MonadMalgo m => Type -> m Type
inst1 (TyForall vs ty) = do
  newTypes <- mapM (\_ -> TyMeta <$> newUniq) vs
  let subst = Subst $ fromList $ zip vs newTypes
  pure $ apply subst ty
inst1 t = pure t

instantiate :: MonadMalgo f => Type -> f Type
instantiate (TyMeta a      ) = pure $ TyMeta a
instantiate (TyApp    c  ts) = TyApp c <$> mapM instantiate ts
instantiate (TyForall vs t ) = do
  ts <- mapM (\_ -> TyMeta <$> newUniq) vs
  let subst = Subst $ fromList $ zip vs ts
  t' <- instantiate t
  pure $ apply subst t'

unify :: MonadMalgo m => Type -> Type -> ExceptT UnifyError m Subst
unify (TyMeta a) t          = bind a t
unify t          (TyMeta a) = bind a t
unify (TyApp c1 ts1) (TyApp c2 ts2) | c1 == c2  = unifyMany ts1 ts2
                                    | otherwise = hoistEither $ Left $ MismatchConstructor c1 c2
unify (TyForall ts1 t1) (TyForall ts2 t2) | length ts1 == length ts2 = do
  let subst = Subst $ fromList $ zipWith (\v1 v2 -> (v1, TyMeta v2)) ts1 ts2
  unify (apply subst t1) (apply subst t2)
unify t1 t2 = hoistEither $ Left $ MismatchLevel t1 t2

unifyMany :: MonadMalgo m => [Type] -> [Type] -> ExceptT UnifyError m Subst
unifyMany []         []         = return mempty
unifyMany (t1 : ts1) (t2 : ts2) = do
  s1 <- unify t1 t2
  s2 <- unifyMany (apply s1 ts1) (apply s1 ts2)
  return $ s2 <> s1
unifyMany ts1 ts2 = hoistEither $ Left $ MismatchLength ts1 ts2

bind :: Monad m => TyVar -> Type -> ExceptT UnifyError m Subst
bind a t | t == TyMeta a   = return mempty
         | occursCheck a t = hoistEither $ Left $ InfinitType a t
         | otherwise       = return $ Subst (one (a, t))

occursCheck :: Substitutable a => TyVar -> a -> Bool
occursCheck a t = a `member` ftv t
