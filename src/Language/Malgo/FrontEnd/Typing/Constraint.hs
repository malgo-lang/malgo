{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
module Language.Malgo.FrontEnd.Typing.Constraint
  ( Constraint(..)
  , solve
  , instantiate
  )
where

import           Language.Malgo.Monad
import           Language.Malgo.Prelude
import           Language.Malgo.Pretty

import           Language.Malgo.TypeRep.Type

import           Language.Malgo.FrontEnd.Typing.Subst

import qualified Data.Map                             as Map

infixl 5 :~
data Constraint = Type :~ Type
    deriving stock (Eq, Show)

instance Substitutable Constraint where
  apply s (t1 :~ t2) = apply s t1 :~ apply s t2
  ftv (t1 :~ t2) = ftv t1 <> ftv t2

solve :: [Constraint] -> Either Doc Subst
solve cs = solver (mempty, cs)

solver :: (Subst, [Constraint]) -> Either Doc Subst
solver (su, []             ) = pure su
solver (su, (t1 :~ t2) : cs) = do
  su1 <- unify t1 t2
  solver (su1 <> su, apply su1 cs)

instantiate :: MonadUniq m => Scheme -> m Type
instantiate (Forall vs t) = do
  ts <- mapM (\_ -> TyMeta <$> getUniq) vs
  pure $ apply (Subst $ Map.fromList $ zip vs ts) t

unify :: Type -> Type -> Either Doc Subst
unify Kind       Kind       = pure mempty
unify Kind       t          = Left $ "mismatch level" <+> pPrint Kind <+> "," <+> pPrint t
unify t          Kind       = Left $ "mismatch level" <+> pPrint Kind <+> "," <+> pPrint t
unify (TyMeta a) t          = bind a t
unify t          (TyMeta a) = bind a t
unify (TyApp c1 ts1) (TyApp c2 ts2)
  | c1 == c2  = unifyMany ts1 ts2 ts1 ts2
  | otherwise = Left $ "mismatch constructor" <+> pPrint c1 <> "," <+> pPrint c2
unify (ps0 :-> r0) (ps1 :-> r1) = unifyMany ps0 ps1 (r0:ps0) (r1:ps1)
unify t1@(TyApp _ _) t2@(_ :-> _) = Left $ "mismatch type" <+> pPrint t1 <> "," <+> pPrint t2
unify t1@(_ :-> _) t2@(TyApp _ _) = Left $ "mismatch type" <+> pPrint t1 <> "," <+> pPrint t2

unifyMany :: [Type] -> [Type] -> [Type] -> [Type] -> Either Doc Subst
unifyMany _ _ []       []       = pure mempty
unifyMany t1 t2 (x : xs) (y : ys) = do
  s1 <- unify x y
  s2 <- unifyMany t1 t2 (apply s1 xs) (apply s1 ys)
  pure $ s2 <> s1
unifyMany t1 t2 _ _ = Left $ "mismatch length" <+> pPrint t1 <> "," <+> pPrint t2

bind :: TyVar -> Type -> Either Doc Subst
bind a t | t == TyMeta a   = pure mempty
         | occursCheck a t = Left $ "infinit type" <+> pPrint a <> "," <+> pPrint t
         | otherwise       = pure $ Subst (Map.singleton a t)

occursCheck :: Substitutable a => TyVar -> a -> Bool
occursCheck a t = ftv t ^. contains a
