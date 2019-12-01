{-# LANGUAGE NoImplicitPrelude #-}
module Language.Malgo.FrontEnd.Typing.Constraint (Constraint(..), solve, UnifyError(..)) where

import           Language.Malgo.FrontEnd.Typing.Subst
import           Language.Malgo.TypeRep.Type
import           Relude                               hiding (Constraint, Type)
import           Relude.Extra.Map

data Constraint = Type :~ Type
  deriving (Eq, Show)

instance Substitutable Constraint where
  apply s (t1 :~ t2) = apply s t1 :~ apply s t2
  ftv (t1 :~ t2) = ftv t1 <> ftv t2

data UnifyError = MismatchConstructor TyCon TyCon
                | MismatchLength [Type] [Type]
                | InfinitType TyVar Type
  deriving Show

solve :: [Constraint] -> Either UnifyError Subst
solve cs = solver (mempty, cs)

solver :: (Subst, [Constraint]) -> Either UnifyError Subst
solver (su, []) = return su
solver (su, (t1 :~ t2) : cs) = do
  su1 <- unify t1 t2
  solver (su1 <> su, apply su1 cs)

unify :: Type -> Type -> Either UnifyError Subst
unify (TyMeta a) t = bind a t
unify t (TyMeta a) = bind a t
unify (TyApp (FunC t1) ts1) (TyApp (FunC t2) ts2) = unifyMany (t1 : ts1) (t2 : ts2)
unify (TyApp c1 ts1) (TyApp c2 ts2)
  | c1 == c2 = unifyMany ts1 ts2
  | otherwise = Left $ MismatchConstructor c1 c2

unifyMany :: [Type] -> [Type] -> Either UnifyError Subst
unifyMany [] [] = return mempty
unifyMany (t1:ts1) (t2:ts2) = do
  s1 <- unify t1 t2
  s2 <- unifyMany (apply s1 ts1) (apply s1 ts2)
  return $ s2 <> s1
unifyMany ts1 ts2 = Left $ MismatchLength ts1 ts2

bind :: TyVar -> Type -> Either UnifyError Subst
bind a t
  | t == TyMeta a = return mempty
  | occursCheck a t = Left $ InfinitType a t
  | otherwise = return $ Subst (one (a, t))

occursCheck :: Substitutable a => TyVar -> a -> Bool
occursCheck a t = a `member` ftv t
