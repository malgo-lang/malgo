{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.FrontEnd.Typing.Constraint
  ( Constraint(..)
  , solve
  , UnifyError(..)
  -- , inst1
  , instantiate
  , throw
  , catchUnifyError
  )
where

import           Language.Malgo.Monad
import           Language.Malgo.Prelude
import           Language.Malgo.Pretty

import           Language.Malgo.TypeRep.Type

import           Language.Malgo.FrontEnd.Info
import           Language.Malgo.FrontEnd.Typing.Subst

import           Control.Monad.Error.Class
import           Text.PrettyPrint.HughesPJClass ( ($+$) )

infixl 5 :~
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

throw :: Info -> Doc -> b
throw info mes = errorDoc $ "error(typing):" <+> pPrint info $+$ mes

catchUnifyError :: Info -> Doc -> Either UnifyError a -> a
catchUnifyError i name (Left (MismatchConstructor c1 c2)) =
  throw i $ "mismatch constructor" <+> pPrint c1 <> "," <+> pPrint c2 $+$ "on" <+> name
catchUnifyError i name (Left (MismatchLength ts1 ts2)) =
  throw i $ "mismatch length" <+> pPrint ts1 <> "," <+> pPrint ts2 $+$ "on" <+> name
catchUnifyError i name (Left (InfinitType var ty)) =
  throw i $ "infinit type" <+> pPrint var <> "," <+> pPrint ty $+$ "on" <+> name
catchUnifyError i name (Left (MismatchLevel ty1 ty2)) =
  throw i $ "mismatch level" <+> pPrint ty1 <> "," <+> pPrint ty2 $+$ "on" <+> name
catchUnifyError _ _ (Right a) = a

solve :: [Constraint] -> Either UnifyError Subst
solve cs = runIdentity $ runExceptT $ solver (mempty, cs)

solver :: MonadError UnifyError m => (Subst, [Constraint]) -> m Subst
solver (su, []             ) = pure su
solver (su, (t1 :~ t2) : cs) = do
  su1 <- unify t1 t2
  solver (su1 <> su, apply su1 cs)

instantiate :: MonadUniq f => Scheme -> f Type
instantiate (Forall vs t) = do
  ts <- mapM (\_ -> TyMeta <$> getUniq) vs
  pure $ apply (Subst $ fromList $ zip vs ts) t

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

bind :: MonadError UnifyError m => TyVar -> Type -> m Subst
bind a t | t == TyMeta a   = pure mempty
         | occursCheck a t = throwError $ InfinitType a t
         | otherwise       = pure $ Subst (one (a, t))

occursCheck :: Substitutable a => TyVar -> a -> Bool
occursCheck a t = a `member` ftv t
