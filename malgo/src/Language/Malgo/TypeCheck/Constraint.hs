{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.TypeCheck.Constraint (Constraint (..), WithPos (..), eqCons, solve) where

import Koriel.Pretty
import Language.Malgo.Prelude
import Language.Malgo.Type
import Text.Megaparsec.Pos (SourcePos)

-- Definition of Constraint
infixl 5 :~

data Constraint = Type :~ Type
  deriving stock (Eq, Show)

instance Pretty Constraint where
  pPrint (t1 :~ t2) = pPrint t1 <+> "~" <+> pPrint t2

data WithPos = WithPos Constraint SourcePos
  deriving stock (Eq, Show)

instance Pretty WithPos where
  pPrint (WithPos c pos) = pPrint pos <> ":" <> pPrint c

eqCons :: SourcePos -> Type -> Type -> WithPos
eqCons pos t1 t2 = WithPos (t1 :~ t2) pos

-- Constraint Solver
solve :: (MonadIO m, MonadMalgo m) => [WithPos] -> m ()
solve cs = solveLoop 5000 =<< zonkConstraints cs

unifyErrorMessage :: (Pretty a1, Pretty a2) => a1 -> a2 -> Doc
unifyErrorMessage t1 t2 = "Couldn't match type" $$ nest 7 (pPrint t1) $$ nest 2 ("with" <+> pPrint t2)

solveLoop :: (MonadIO m, MonadMalgo m) => Int -> [WithPos] -> m ()
solveLoop n _ | n <= 0 = error "Constraint solver error: iteration limit"
solveLoop _ [] = pure ()
solveLoop n (WithPos (TyMeta a1 :~ TyMeta a2) pos : cs)
  | a1 == a2 = solveLoop (n - 1) cs
  | isRigid a1 && isRigid a2 && rigidName a1 /= rigidName a2 =
    errorOn pos $
      unifyErrorMessage a1 a2
        $+$ quotes (pPrint a1) <+> "and" <+> quotes (pPrint a2) <+> "are rigid type variables"
  | isRigid a1 = do
    bind pos a2 (TyMeta a1)
    solveLoop (n - 1) =<< zonkConstraints cs
  | isRigid a2 = do
    bind pos a1 (TyMeta a2)
    solveLoop (n - 1) =<< zonkConstraints cs
solveLoop n (WithPos (TyMeta a :~ t) pos : cs)
  | isRigid a = errorOn pos $ unifyErrorMessage a t $+$ quotes (pPrint a) <+> "is a rigid type variable"
  | otherwise = do
    bind pos a t
    solveLoop (n - 1) =<< zonkConstraints cs
solveLoop n (WithPos (t :~ TyMeta a) pos : cs)
  | isRigid a = errorOn pos $ unifyErrorMessage a t $+$ quotes (pPrint a) <+> "is a rigid type variable"
  | otherwise = do
    bind pos a t
    solveLoop (n - 1) =<< zonkConstraints cs
solveLoop n (WithPos (TyApp t11 t12 :~ TyApp t21 t22) pos : cs) =
  solveLoop (n - 1) $ WithPos (t11 :~ t21) pos : WithPos (t12 :~ t22) pos : cs
solveLoop n (WithPos (TyArr t11 t12 :~ TyArr t21 t22) pos : cs) =
  solveLoop (n - 1) $ WithPos (t11 :~ t21) pos : WithPos (t12 :~ t22) pos : cs
solveLoop n (WithPos (TyTuple ts1 :~ TyTuple ts2) pos : cs) =
  solveLoop (n - 1) $ zipWith (\t1 t2 -> WithPos (t1 :~ t2) pos) ts1 ts2 <> cs
solveLoop n (WithPos (TyLazy t1 :~ TyLazy t2) pos : cs) =
  solveLoop (n - 1) $ WithPos (t1 :~ t2) pos : cs
solveLoop n (WithPos (TyPtr t1 :~ TyPtr t2) pos : cs) =
  solveLoop (n - 1) $ WithPos (t1 :~ t2) pos : cs
solveLoop n (WithPos (t1 :~ t2) pos : cs)
  | t1 == t2 = solveLoop (n - 1) cs
  | otherwise = errorOn pos $ unifyErrorMessage t1 t2

bind :: (MonadMalgo m, MonadIO m) => SourcePos -> MetaTv -> Type -> m ()
bind pos tv t2 = do
  mktv <- kind tv
  mkt2 <- kind t2
  case (mktv, mkt2) of
    (Just ktv, Just kt2) | ktv /= kt2 -> errorOn pos $ "Kind mismatch:" <+> vcat [quotes (pPrint tv) <> ":" <> pPrint ktv, pPrint t2 <> ":" <> pPrint kt2]
    _ -> do
      mt1 <- readMetaTv tv
      case mt1 of
        Just t1 -> errorOn pos $ "Internal Error:" <+> quotes (pPrint tv) <+> "is already bound to" <+> quotes (pPrint t1)
        Nothing -> do
          if tv `elem` metaTvs t2
            then errorOn pos $ "Occurs check" <+> quotes (pPrint tv) <+> "for" <+> pPrint t2
            else writeMetaTv tv t2

zonkConstraints :: (Traversable t, MonadIO f) => t WithPos -> f (t WithPos)
zonkConstraints = traverse zonkConstraint

zonkConstraint :: MonadIO m => WithPos -> m WithPos
zonkConstraint (WithPos (t1 :~ t2) pos) = do
  t1' <- zonkType t1
  t2' <- zonkType t2
  pure (WithPos (t1' :~ t2') pos)