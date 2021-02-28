{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.TypeCheck.Constraint (Constraint (..), WithPos, eqCons, solve) where

import Koriel.Pretty
import Language.Malgo.Prelude
import Language.Malgo.TypeRep.IORef
import Text.Megaparsec.Pos (SourcePos)

-- Definition of Constraint
infixl 5 :~

data Constraint = Type :~ Type
  deriving stock (Eq, Show)

instance Pretty Constraint where
  pPrint (t1 :~ t2) = pPrint t1 <+> "~" <+> pPrint t2

type WithPos = With SourcePos Constraint

eqCons :: SourcePos -> Type -> Type -> WithPos
eqCons pos t1 t2 = With pos (t1 :~ t2)

-- Constraint Solver
solve :: (MonadIO m, MonadMalgo m) => [WithPos] -> m ()
solve cs = solveLoop 5000 =<< zonkConstraints cs

unifyErrorMessage :: (Pretty a1, Pretty a2) => a1 -> a2 -> Doc
unifyErrorMessage t1 t2 = "Couldn't match type" $$ nest 7 (pPrint t1) $$ nest 2 ("with" <+> pPrint t2)

solveLoop :: (MonadIO m, MonadMalgo m) => Int -> [WithPos] -> m ()
solveLoop n _ | n <= 0 = error "Constraint solver error: iteration limit"
solveLoop _ [] = pure ()
solveLoop n (With pos (TyMeta a1 :~ TyMeta a2) : cs)
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
solveLoop n (With pos (TyMeta a :~ t) : cs)
  | isRigid a = errorOn pos $ unifyErrorMessage a t $+$ quotes (pPrint a) <+> "is a rigid type variable"
  | otherwise = do
    bind pos a t
    solveLoop (n - 1) =<< zonkConstraints cs
solveLoop n (With pos (t :~ TyMeta a) : cs)
  | isRigid a = errorOn pos $ unifyErrorMessage a t $+$ quotes (pPrint a) <+> "is a rigid type variable"
  | otherwise = do
    bind pos a t
    solveLoop (n - 1) =<< zonkConstraints cs
solveLoop n (With pos (TyApp t11 t12 :~ TyApp t21 t22) : cs) =
  solveLoop (n - 1) $ With pos (t11 :~ t21) : With pos (t12 :~ t22) : cs
solveLoop n (With pos (TyArr t11 t12 :~ TyArr t21 t22) : cs) =
  solveLoop (n - 1) $ With pos (t11 :~ t21) : With pos (t12 :~ t22) : cs
solveLoop n (With pos (TyTuple ts1 :~ TyTuple ts2) : cs) =
  solveLoop (n - 1) $ zipWith (\t1 t2 -> With pos (t1 :~ t2)) ts1 ts2 <> cs
solveLoop n (With pos (TyLazy t1 :~ TyLazy t2) : cs) =
  solveLoop (n - 1) $ With pos (t1 :~ t2) : cs
solveLoop n (With pos (TyPtr t1 :~ TyPtr t2) : cs) =
  solveLoop (n - 1) $ With pos (t1 :~ t2) : cs
solveLoop n (With pos (t1 :~ t2) : cs)
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
zonkConstraint (With pos (t1 :~ t2)) = do
  t1' <- zonkType t1
  t2' <- zonkType t2
  pure (With pos (t1' :~ t2'))