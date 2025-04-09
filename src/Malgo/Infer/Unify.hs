{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Unification
module Malgo.Infer.Unify
  ( Constraint (..),
    lookupVar,
    freshVar,
    bindVar,
    zonk,
    solve,
    generalize,
    generalizeMutRecs,
    instantiate,
  )
where

import Control.Lens (itraverse_)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Traversable (for)
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Effectful.State.Static.Local
import GHC.Records (HasField)
import Malgo.Id
import Malgo.Infer.Error
import Malgo.Infer.Kind
import Malgo.Infer.TcEnv (TcEnv (..))
import Malgo.Infer.TypeRep
import Malgo.Module
import Malgo.MonadUniq
import Malgo.Prelude hiding (Constraint, throwError)
import Prettyprinter ((<+>))

-- * Constraint

infixl 5 :~

-- | Constraint
-- a :~ b means 'a ~ b'
data Constraint = Type :~ Type
  deriving stock (Eq, Ord, Show, Generic)

instance Pretty Constraint where
  pretty (t1 :~ t2) = pretty t1 <+> "~" <+> pretty t2

-- * Unifiable

lookupVar :: (State TypeMap :> es) => MetaVar -> Eff es (Maybe Type)
lookupVar v = Map.lookup v <$> get @TypeMap

freshVar ::
  (State Uniq :> es, Reader ModuleName :> es, State KindCtx :> es) =>
  Maybe Text ->
  Eff es MetaVar
freshVar hint = do
  hint <- pure $ fromMaybe "t" hint
  kind <- newTemporalId ("k" <> hint)
  newVar <- newInternalId hint
  modify (insertKind newVar (TyMeta $ MetaVar kind))
  pure $ MetaVar newVar

bindVar :: (IOE :> es, State TcEnv :> es, State TypeMap :> es, Reader Flag :> es, Error InferError :> es, State KindCtx :> es) => Range -> MetaVar -> Type -> Eff es ()
bindVar range v t = do
  when (occursCheck v t) do
    throwError $ OccursCheckFailed range v t
  ctx <- get
  vKind <- kindOf range ctx v.metaVar
  tKind <- kindOf range ctx t
  solve range [(range, vKind :~ tKind)]
  modify @TypeMap (Map.insert v t)
  where
    occursCheck :: MetaVar -> Type -> Bool
    occursCheck v t = Set.member v (freevars t)

zonk :: (State TypeMap :> es, State TcEnv :> es, State KindCtx :> es, Error InferError :> es) => Range -> Type -> Eff es Type
zonk range (TyApp t1 t2) = TyApp <$> zonk range t1 <*> zonk range t2
zonk range (TyVar v) = do
  ctx <- get
  k <- zonk range =<< kindOf range ctx v
  modify (insertKind v k)
  pure $ TyVar v
zonk range (TyCon c) = do
  ctx <- get
  k <- zonk range =<< kindOf range ctx c
  modify (insertKind c k)
  pure $ TyCon c
zonk _ t@TyPrim {} = pure t
zonk range (TyArr t1 t2) = TyArr <$> zonk range t1 <*> zonk range t2
zonk _ t@TyTuple {} = pure t
zonk range (TyRecord kts) = TyRecord <$> traverse (zonk range) kts
zonk _ TyPtr = pure TyPtr
zonk _ TYPE = pure TYPE
zonk range t@(TyMeta v) = fromMaybe t <$> (traverse (zonk range) =<< lookupVar v)

-- | 'Right' (substituation, new constraints) or 'Left' (position, error message)
type UnifyResult ann = Either InferError (Map MetaVar Type, [(Range, Constraint)])

-- | Unify two types
unify :: Range -> Type -> Type -> UnifyResult ann
unify _ (TyMeta v1) (TyMeta v2)
  | v1 == v2 = pure (mempty, [])
  | otherwise = pure (Map.singleton v1 (TyMeta v2), [])
unify _ (TyMeta v) t = pure (Map.singleton v t, [])
unify _ t (TyMeta v) = pure (Map.singleton v t, [])
unify x (TyApp t11 t12) (TyApp t21 t22) = pure (mempty, [(x, t11 :~ t21), (x, t12 :~ t22)])
unify _ (TyVar v1) (TyVar v2) | v1 == v2 = pure (mempty, [])
unify _ (TyCon c1) (TyCon c2) | c1 == c2 = pure (mempty, [])
unify _ (TyPrim p1) (TyPrim p2) | p1 == p2 = pure (mempty, [])
unify x (TyArr l1 r1) (TyArr l2 r2) = pure (mempty, [(x, l1 :~ l2), (x, r1 :~ r2)])
unify _ (TyTuple n1) (TyTuple n2) | n1 == n2 = pure (mempty, [])
unify x (TyRecord kts1) (TyRecord kts2)
  | Map.keys kts1 == Map.keys kts2 = pure (mempty, zipWith (\t1 t2 -> (x, t1 :~ t2)) (Map.elems kts1) (Map.elems kts2))
unify _ TyPtr TyPtr = pure (mempty, [])
unify _ TYPE TYPE = pure (mempty, [])
unify x t1 t2 = Left $ UnificationError x t1 t2

-- * Constraint solver

solve :: (State TypeMap :> es, State TcEnv :> es, IOE :> es, Reader Flag :> es, Error InferError :> es, State KindCtx :> es) => Range -> [(Range, Constraint)] -> Eff es ()
solve range = solveLoop (5000 :: Int)
  where
    solveLoop n _ | n <= 0 = throwError $ IterationLimitExceeded range
    solveLoop _ [] = pass
    solveLoop n ((x, t1 :~ t2) : cs) = do
      abbrEnv <- gets @TcEnv (.typeSynonymMap)
      let t1' = fromMaybe t1 (expandTypeSynonym abbrEnv t1)
      let t2' = fromMaybe t2 (expandTypeSynonym abbrEnv t2)
      case unify x t1' t2' of
        Left err -> throwError err
        Right (binds, cs') -> do
          itraverse_ (bindVar x) binds
          constraints <- traverse (zonkConstraint range) (cs' <> cs)
          solveLoop (n - 1) constraints
    zonkConstraint range (m, x :~ y) = (m,) <$> ((:~) <$> zonk range x <*> zonk range y)

generalize ::
  (State TypeMap :> es, State TcEnv :> es, IOE :> es, Reader Flag :> es, Error InferError :> es, State KindCtx :> es) =>
  Range ->
  Type ->
  Eff es (Scheme Type)
generalize range term = do
  zonkedTerm <- zonk range term
  let fvs = Set.toList $ freevars zonkedTerm
  let as = map toBound fvs
  zipWithM_ (\fv a -> bindVar range fv $ TyVar a) fvs as
  Forall as <$> zonk range zonkedTerm

generalizeMutRecs ::
  (State TypeMap :> es, State TcEnv :> es, IOE :> es, Reader Flag :> es, Error InferError :> es, State KindCtx :> es) =>
  Range ->
  [Type] ->
  Eff es ([TypeVar], [Type])
generalizeMutRecs range terms = do
  zonkedTerms <- traverse (zonk range) terms
  let fvs = Set.toList $ mconcat $ map freevars zonkedTerms
  let as = map toBound fvs
  zipWithM_ (\fv a -> bindVar range fv $ TyVar a) fvs as
  (as,) <$> traverse (zonk range) zonkedTerms

-- `toBound` "generates" a new bound variable from a free variable.
-- But it's not really generating a new variable, it's just using the free variable as a bound variable.
-- The free variable will zonk to the bound variable as soon as the bound variable is bound (`bindVar`).
-- So we can reuse the free variable as a bound variable.
toBound :: (HasField "metaVar" r a) => r -> a
toBound tv = tv.metaVar

instantiate :: (Reader ModuleName :> es, State Uniq :> es, State TcEnv :> es, State TypeMap :> es, IOE :> es, Reader Flag :> es, Error InferError :> es, State KindCtx :> es) => Range -> Scheme Type -> Eff es Type
instantiate range (Forall as t) = do
  avs <- for as \a -> do
    v <- TyMeta <$> freshVar (Just a.name)
    ctx <- get
    aKind <- kindOf range ctx a
    vKind <- kindOf range ctx v
    solve range [(range, aKind :~ vKind)]
    pure (a, v)
  pure $ applySubst (Map.fromList avs) t
