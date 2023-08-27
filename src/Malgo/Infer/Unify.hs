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
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Traversable (for)
import Effectful
import Effectful.Reader.Static
import Effectful.State.Static.Local
import GHC.Records (HasField)
import Koriel.Id
import Koriel.Lens (kindCtx)
import Koriel.MonadUniq
import Koriel.Pretty
import Malgo.Infer.TcEnv (TcEnv (..))
import Malgo.Infer.TypeRep
import Malgo.Prelude hiding (Constraint)

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
lookupVar v = HashMap.lookup v <$> get @TypeMap

freshVar ::
  (State Uniq :> es, Reader ModuleName :> es, State TcEnv :> es) =>
  Maybe Text ->
  Eff es MetaVar
freshVar hint = do
  hint <- pure $ fromMaybe "t" hint
  kind <- newTemporalId ("k" <> hint) ()
  newVar <- newInternalId hint ()
  modify @TcEnv (over kindCtx (insertKind newVar (TyMeta $ MetaVar kind)))
  pure $ MetaVar newVar

bindVar :: (IOE :> es, State TcEnv :> es, State TypeMap :> es, Reader Flag :> es) => Range -> MetaVar -> Type -> Eff es ()
bindVar x v t = do
  when (occursCheck v t) $ errorOn x $ "Occurs check:" <+> squotes (pretty v) <+> "for" <+> pretty t
  ctx <- gets @TcEnv (._kindCtx)
  solve [(x, kindOf ctx v.metaVar :~ kindOf ctx t)]
  modify @TypeMap (HashMap.insert v t)
  where
    occursCheck :: MetaVar -> Type -> Bool
    occursCheck v t = HashSet.member v (freevars t)

zonk :: (State TypeMap :> es, State TcEnv :> es) => Type -> Eff es Type
zonk (TyApp t1 t2) = TyApp <$> zonk t1 <*> zonk t2
zonk (TyVar v) = do
  ctx <- gets @TcEnv (._kindCtx)
  k <- zonk $ kindOf ctx v
  modify @TcEnv (over kindCtx (insertKind v k))
  pure $ TyVar v
zonk (TyCon c) = do
  ctx <- gets @TcEnv (._kindCtx)
  k <- zonk $ kindOf ctx c
  modify @TcEnv (over kindCtx (insertKind c k))
  pure $ TyCon c
zonk t@TyPrim {} = pure t
zonk (TyArr t1 t2) = TyArr <$> zonk t1 <*> zonk t2
zonk t@TyTuple {} = pure t
zonk (TyRecord kts) = TyRecord <$> traverse zonk kts
zonk TyPtr = pure TyPtr
zonk TYPE = pure TYPE
zonk t@(TyMeta v) = fromMaybe t <$> (traverse zonk =<< lookupVar v)

-- | 'Right' (substituation, new constraints) or 'Left' (position, error message)
type UnifyResult ann = Either (Range, Doc ann) (HashMap MetaVar Type, [(Range, Constraint)])

-- | Unify two types
unify :: Range -> Type -> Type -> UnifyResult ann
unify _ (TyMeta v1) (TyMeta v2)
  | v1 == v2 = pure (mempty, [])
  | otherwise = pure (HashMap.singleton v1 (TyMeta v2), [])
unify _ (TyMeta v) t = pure (HashMap.singleton v t, [])
unify _ t (TyMeta v) = pure (HashMap.singleton v t, [])
unify x (TyApp t11 t12) (TyApp t21 t22) = pure (mempty, [(x, t11 :~ t21), (x, t12 :~ t22)])
unify _ (TyVar v1) (TyVar v2) | v1 == v2 = pure (mempty, [])
unify _ (TyCon c1) (TyCon c2) | c1 == c2 = pure (mempty, [])
unify _ (TyPrim p1) (TyPrim p2) | p1 == p2 = pure (mempty, [])
unify x (TyArr l1 r1) (TyArr l2 r2) = pure (mempty, [(x, l1 :~ l2), (x, r1 :~ r2)])
unify _ (TyTuple n1) (TyTuple n2) | n1 == n2 = pure (mempty, [])
unify x (TyRecord kts1) (TyRecord kts2)
  | HashMap.keys kts1 == HashMap.keys kts2 = pure (mempty, zipWith (\t1 t2 -> (x, t1 :~ t2)) (HashMap.elems kts1) (HashMap.elems kts2))
unify _ TyPtr TyPtr = pure (mempty, [])
unify _ TYPE TYPE = pure (mempty, [])
unify x t1 t2 = Left (x, unifyErrorMessage t1 t2)
  where
    unifyErrorMessage t1 t2 = vsep ["Couldn't match", nest 7 (pretty t1), nest 2 ("with" <+> pretty t2)]

-- * Constraint solver

solve :: (State TypeMap :> es, State TcEnv :> es, IOE :> es, Reader Flag :> es) => [(Range, Constraint)] -> Eff es ()
solve = solveLoop (5000 :: Int)
  where
    solveLoop n _ | n <= 0 = error "Constraint solver error: iteration limit"
    solveLoop _ [] = pass
    solveLoop n ((x, t1 :~ t2) : cs) = do
      abbrEnv <- gets @TcEnv (._typeSynonymMap)
      let t1' = fromMaybe t1 (expandTypeSynonym abbrEnv t1)
      let t2' = fromMaybe t2 (expandTypeSynonym abbrEnv t2)
      case unify x t1' t2' of
        Left (pos, message) -> errorOn pos message
        Right (binds, cs') -> do
          itraverse_ (bindVar x) binds
          constraints <- traverse zonkConstraint (cs' <> cs)
          solveLoop (n - 1) constraints
    zonkConstraint (m, x :~ y) = (m,) <$> ((:~) <$> zonk x <*> zonk y)

generalize ::
  (State TypeMap :> es, State TcEnv :> es, IOE :> es, Reader Flag :> es) =>
  Range ->
  Type ->
  Eff es (Scheme Type)
generalize x term = do
  zonkedTerm <- zonk term
  let fvs = HashSet.toList $ freevars zonkedTerm
  let as = map toBound fvs
  zipWithM_ (\fv a -> bindVar x fv $ TyVar a) fvs as
  Forall as <$> zonk zonkedTerm

generalizeMutRecs ::
  (State TypeMap :> es, State TcEnv :> es, IOE :> es, Reader Flag :> es) =>
  Range ->
  [Type] ->
  Eff es ([TypeVar], [Type])
generalizeMutRecs x terms = do
  zonkedTerms <- traverse zonk terms
  let fvs = HashSet.toList $ mconcat $ map freevars zonkedTerms
  let as = map toBound fvs
  zipWithM_ (\fv a -> bindVar x fv $ TyVar a) fvs as
  (as,) <$> traverse zonk zonkedTerms

-- `toBound` "generates" a new bound variable from a free variable.
-- But it's not really generating a new variable, it's just using the free variable as a bound variable.
-- The free variable will zonk to the bound variable as soon as the bound variable is bound (`bindVar`).
-- So we can reuse the free variable as a bound variable.
toBound :: (HasField "metaVar" r a) => r -> a
toBound tv = tv.metaVar

instantiate :: (Reader ModuleName :> es, State Uniq :> es, State TcEnv :> es, State TypeMap :> es, IOE :> es, Reader Flag :> es) => Range -> Scheme Type -> Eff es Type
instantiate x (Forall as t) = do
  avs <- for as \a -> do
    v <- TyMeta <$> freshVar (Just a.name)
    ctx <- gets @TcEnv (._kindCtx)
    solve [(x, kindOf ctx a :~ kindOf ctx v)]
    pure (a, v)
  pure $ applySubst (HashMap.fromList avs) t
