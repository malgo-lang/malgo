{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
module Language.Malgo.FrontEnd.Typing.Constraint
  ( Constraint(..)
  , solve
  , UnifyError(..)
  )
where

import Language.Malgo.Monad
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
  deriving Show

solve :: MonadMalgo m =>
           [Constraint] -> m (Either UnifyError Subst)
solve cs = runExceptT $ solver (mempty, cs)

solver :: MonadMalgo m =>
            (Subst, [Constraint]) -> ExceptT UnifyError m Subst
solver (su, []             ) = return su
solver (su, (t1 :~ t2) : cs) = do
  su1 <- unify t1 t2
  solver (su1 <> su, apply su1 cs)

instantiate :: (MonadMalgo m, Substitutable b) =>
                 [TyVar] -> b -> ExceptT UnifyError m b
instantiate vs ty = do
  newTypes <- mapM (\_ -> TyMeta <$> newUniq) vs
  let cs = map (\(v, t) -> TyMeta v :~ t) (zip vs newTypes)
  s <- solver  (mempty, cs)
  pure $ apply s ty

unify :: MonadMalgo m => Type -> Type -> ExceptT UnifyError m Subst
unify (TyMeta a) t          = bind a t
unify t          (TyMeta a) = bind a t
unify (TyApp c1 ts1) (TyApp c2 ts2)
  | c1 == c2  = unifyMany ts1 ts2
  | otherwise = hoistEither $ Left $ MismatchConstructor c1 c2
unify (TyForall ts t1) t2 = do
  t1' <- instantiate ts t1
  unify t1' t2
unify t1 (TyForall ts t2) = do
  t2' <- instantiate ts t2
  unify t1 t2'

unifyMany :: MonadMalgo m =>
               [Type] -> [Type] -> ExceptT UnifyError m Subst
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
