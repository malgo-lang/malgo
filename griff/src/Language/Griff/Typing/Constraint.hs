{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Typing.Constraint (Constraint (..), WithPos (..), eqCons, solve) where

import qualified Data.Set as Set
import Koriel.Prelude
import Koriel.Pretty
import qualified Koriel.Pretty as P
import Language.Griff.Type
import Text.Megaparsec.Pos (SourcePos)

-- Definition of Constraint
infixl 5 :~

data Constraint = Type :~ Type
  deriving stock (Eq, Show)

data WithPos = WithPos Constraint SourcePos
  deriving stock (Eq, Show)

eqCons :: SourcePos -> Type -> Type -> WithPos
eqCons pos t1 t2 = WithPos (t1 :~ t2) pos

-- Constraint Solver
solve :: MonadIO m => [WithPos] -> m ()
solve = solveLoop 5000

solveLoop :: MonadIO m => Int -> [WithPos] -> m ()
solveLoop n _ | n <= 0 = error "Constraint solver error: iteration limit"
solveLoop _ [] = pure ()
solveLoop n (WithPos (TyMeta a1 :~ TyMeta a2) pos : cs)
  | a1 == a2 = solveLoop (n - 1) cs
  | (isRigit a1 && isRigit a2) && (rigitName a1 /= rigitName a2) = errorOn pos $ "Type mismatch:" <+> P.vcat [pPrint a1, pPrint a2]
  | isRigit a1 = do
    bind pos a2 (TyMeta a1)
    solveLoop (n - 1) =<< zonkConstraints cs
  | isRigit a2 = do
    bind pos a1 (TyMeta a2)
    solveLoop (n - 1) =<< zonkConstraints cs
solveLoop n (WithPos (TyMeta a :~ t) pos : cs) = do
  bind pos a t
  solveLoop (n - 1) =<< zonkConstraints cs
solveLoop n (WithPos (t :~ TyMeta a) pos : cs) = do
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
solveLoop n (WithPos (t1 :~ t2) pos : cs)
  | t1 == t2 = solveLoop (n - 1) cs
  | otherwise = errorOn pos $ "Type mismatch:" <+> P.vcat [pPrint t1, pPrint t2]

metaTvs :: Type -> Set MetaTv
metaTvs (TyApp t1 t2) = metaTvs t1 <> metaTvs t2
metaTvs (TyArr t1 t2) = metaTvs t1 <> metaTvs t2
metaTvs (TyTuple ts) = mconcat $ map metaTvs ts
metaTvs (TyLazy t) = metaTvs t
metaTvs (TyMeta tv) = Set.singleton tv
metaTvs _ = mempty

bind :: (MonadIO m, Pretty a) => a -> MetaTv -> Type -> m ()
bind pos tv t2
  | kind tv /= kind t2 = errorOn pos $ "Kind mismatch:" <+> vcat [quotes $ pPrint tv, pPrint t2]
  | otherwise = do
    mt1 <- readMetaTv tv
    case mt1 of
      Just _ -> errorOn pos "Internal Error"
      Nothing -> do
        if tv `elem` metaTvs t2
          then errorOn pos $ "Occurs check" <+> P.quotes (pPrint tv) <+> "for" <+> pPrint t2
          else writeMetaTv tv t2

zonkConstraints :: (Traversable t, MonadIO f) => t WithPos -> f (t WithPos)
zonkConstraints = traverse zonkConstraint

zonkConstraint :: MonadIO m => WithPos -> m WithPos
zonkConstraint (WithPos (t1 :~ t2) pos) = do
  t1' <- zonkType t1
  t2' <- zonkType t2
  pure (WithPos (t1' :~ t2') pos)