{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.FrontEnd.Typing.Constraint
  ( Constraint (..),
    WithPos (..),
    solve,
    instantiate,
  )
where

import qualified Data.Map as Map
import Language.Malgo.FrontEnd.Typing.Subst
import Language.Malgo.Monad
import Language.Malgo.Prelude
import Language.Malgo.Pretty
import Language.Malgo.TypeRep.Type
import Text.Parsec.Pos (SourcePos)
import Text.PrettyPrint (($$))

infixl 5 :~

data Constraint = Type :~ Type
  deriving stock (Eq, Show)

instance Substitutable Constraint where
  apply s (t1 :~ t2) = apply s t1 :~ apply s t2
  ftv (t1 :~ t2) = ftv t1 <> ftv t2

data WithPos = WithPos Constraint SourcePos
  deriving stock (Eq, Show)

instance Substitutable WithPos where
  apply s (WithPos c pos) = WithPos (apply s c) pos
  ftv (WithPos c _) = ftv c

solve :: [WithPos] -> Either Doc Subst
solve cs = solver (mempty, cs)

solver :: (Subst, [WithPos]) -> Either Doc Subst
solver (su, []) = pure su
solver (su, (WithPos (t1 :~ t2) pos) : cs) = do
  su1 <- unify pos t1 t2
  solver (su1 <> su, apply su1 cs)

instantiate :: MonadUniq m => Scheme -> m Type
instantiate (Forall vs t) = do
  ts <- mapM (\_ -> TyMeta <$> getUniq) vs
  pure $ apply (Subst $ Map.fromList $ zip vs ts) t

unify :: SourcePos -> Type -> Type -> Either Doc Subst
unify pos (TyMeta a) t = bind pos a t
unify pos t (TyMeta a) = bind pos a t
unify pos (TyApp c1 ts1) (TyApp c2 ts2)
  | c1 == c2 = unifyMany pos ts1 ts2 ts1 ts2
  | otherwise = Left $ "mismatch constructor" <+> pPrint c1 <> "," <+> pPrint c2 $$ "on:" <+> pPrint pos
unify pos (ps0 :-> r0) (ps1 :-> r1) = unifyMany pos ps0 ps1 (r0 : ps0) (r1 : ps1)
unify pos t1@(TyApp _ _) t2@(_ :-> _) = Left $ "mismatch type" <+> pPrint t1 <> "," <+> pPrint t2 $$ "on:" <+> pPrint pos
unify pos t1@(_ :-> _) t2@(TyApp _ _) = Left $ "mismatch type" <+> pPrint t1 <> "," <+> pPrint t2 $$ "on:" <+> pPrint pos

unifyMany :: SourcePos -> [Type] -> [Type] -> [Type] -> [Type] -> Either Doc Subst
unifyMany _ _ _ [] [] = pure mempty
unifyMany pos t1 t2 (x : xs) (y : ys) = do
  s1 <- unify pos x y
  s2 <- unifyMany pos t1 t2 (apply s1 xs) (apply s1 ys)
  pure $ s2 <> s1
unifyMany pos t1 t2 _ _ = Left $ "mismatch length" <+> pPrint t1 <> "," <+> pPrint t2 $$ "on:" <+> pPrint pos

bind :: SourcePos -> TyVar -> Type -> Either Doc Subst
bind pos a t
  | t == TyMeta a = pure mempty
  | occursCheck a t = Left $ "infinit type" <+> pPrint a <> "," <+> pPrint t $$ "on:" <+> pPrint pos
  | otherwise = pure $ Subst (Map.singleton a t)

occursCheck :: Substitutable a => TyVar -> a -> Bool
occursCheck a t = ftv t ^. contains a
