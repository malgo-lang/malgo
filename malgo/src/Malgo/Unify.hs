{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Unification
module Malgo.Unify where

import Control.Monad.Except (ExceptT)
import Data.Functor.Classes
import qualified Data.HashSet as HashSet
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Pretty
import Malgo.Prelude
import Malgo.TypeRep.Static hiding (applySubst, kindOf)
import Malgo.TypeRep.UTerm
import Malgo.UTerm
import qualified RIO.HashMap as HashMap
import qualified RIO.List as List
import qualified RIO.Map as Map
import Text.Megaparsec (SourcePos)

----------------
-- Constraint --
----------------

infixl 5 :~

-- | Constraint
-- a :~ b means 'a ~ b'
data Constraint t = t :~ t
  deriving stock (Eq, Ord, Show, Generic)

instance (Pretty t) => Pretty (Constraint t) where
  pPrint (t1 :~ t2) = pPrint t1 <+> "~" <+> pPrint t2

---------------
-- Unifiable --
---------------

type UnifyResult t = (HashMap (Var t) t, [With SourcePos (Constraint t)])

unifyErrorMessage :: (Pretty a, Pretty b) => a -> b -> Doc
unifyErrorMessage t1 t2 = "Couldn't match" $$ nest 7 (pPrint t1) $$ nest 2 ("with" <+> pPrint t2)

-- | Unifiable value
class (Hashable (Var t), Eq (Var t), Eq t, Pretty t) => Unifiable t where
  -- | Representation of variable
  type Var t

  -- | Unify two terms and generate substituation and new constraints
  unify :: (HasCallStack, MonadReader env m, HasOpt env, MonadIO m) => SourcePos -> t -> t -> m (UnifyResult t)

  -- | Check alpha-equivalence
  equiv :: t -> t -> Maybe (HashMap (Var t) (Var t))

  -- | Free variables
  freevars :: t -> HashSet (Var t)

  -- | Occurs check
  occursCheck :: (Eq (Var t), Hashable (Var t)) => Var t -> t -> Bool
  occursCheck v t = HashSet.member v (freevars t)

-- | Lifted version of Unifiable
class Unifiable1 t where
  liftUnify :: (HasCallStack, MonadReader env m, HasOpt env, MonadIO m, Unifiable a) => (SourcePos -> a -> a -> m (UnifyResult a)) -> SourcePos -> t a -> t a -> m (UnifyResult a)
  liftEquiv :: Unifiable a => (a -> a -> Maybe (HashMap (Var a) (Var a))) -> t a -> t a -> Maybe (HashMap (Var a) (Var a))
  liftFreevars :: Unifiable a => (a -> HashSet (Var a)) -> t a -> HashSet (Var a)
  liftOccursCheck :: Unifiable a => (Var a -> a -> Bool) -> Var a -> t a -> Bool

class (Monad m, Unifiable t) => MonadBind t m where
  lookupVar :: Var t -> m (Maybe t)
  default lookupVar :: (MonadTrans tr, MonadBind t m1, m ~ tr m1) => Var t -> m (Maybe t)
  lookupVar v = lift (lookupVar v)
  freshVar :: m (Var t)
  default freshVar :: (MonadTrans tr, MonadBind t m1, m ~ tr m1) => m (Var t)
  freshVar = lift (freshVar @t)
  bindVar :: SourcePos -> Var t -> t -> m ()
  default bindVar :: (MonadTrans tr, MonadBind t m1, m ~ tr m1) => SourcePos -> Var t -> t -> m ()
  bindVar x v t = lift (bindVar x v t)

  -- Apply all substituation
  zonk :: t -> m t
  default zonk :: (MonadTrans tr, MonadBind t m1, m ~ tr m1) => t -> m t
  zonk t = lift (zonk t)

instance MonadBind t m => MonadBind t (ReaderT r m)

instance MonadBind t m => MonadBind t (ExceptT e m)

instance MonadBind t m => MonadBind t (StateT s m)

instance (Monoid w, MonadBind t m) => MonadBind t (WriterT w m)

instance (Eq v, Hashable v, Unifiable1 t, Eq1 t, Pretty v, Pretty (t (UTerm t v))) => Unifiable (UTerm t v) where
  type Var (UTerm t v) = v
  unify _ (UVar v1) (UVar v2)
    | v1 == v2 = pure (mempty, [])
    | otherwise = pure (HashMap.singleton v1 (UVar v2), [])
  unify _ (UVar v) (UTerm t) = pure (HashMap.singleton v (UTerm t), [])
  unify _ (UTerm t) (UVar v) = pure (HashMap.singleton v (UTerm t), [])
  unify x (UTerm t1) (UTerm t2) = liftUnify unify x t1 t2
  equiv (UVar v1) (UVar v2)
    | v1 == v2 = Just mempty
    | otherwise = Just $ HashMap.singleton v1 v2
  equiv (UTerm t1) (UTerm t2) = liftEquiv equiv t1 t2
  equiv _ _ = Nothing
  freevars (UVar v) = HashSet.singleton v
  freevars (UTerm t) = liftFreevars freevars t

instance Unifiable1 TypeF where
  liftUnify _ x (TyAppF t11 t12) (TyAppF t21 t22) = pure (mempty, [With x $ t11 :~ t21, With x $ t12 :~ t22])
  liftUnify _ _ (TyVarF v1) (TyVarF v2) | v1 == v2 = pure (mempty, [])
  liftUnify _ _ (TyConF c1) (TyConF c2) | c1 == c2 = pure (mempty, [])
  liftUnify _ _ (TyPrimF p1) (TyPrimF p2) | p1 == p2 = pure (mempty, [])
  liftUnify _ x (TyArrF l1 r1) (TyArrF l2 r2) = pure (mempty, [With x $ l1 :~ l2, With x $ r1 :~ r2])
  liftUnify _ _ (TyTupleF n1) (TyTupleF n2) | n1 == n2 = pure (mempty, [])
  liftUnify _ x (TyRecordF kts1) (TyRecordF kts2)
    | Map.keys kts1 == Map.keys kts2 = pure (mempty, zipWith (\t1 t2 -> With x $ t1 :~ t2) (Map.elems kts1) (Map.elems kts2))
  liftUnify _ _ TyLazyF TyLazyF = pure (mempty, [])
  liftUnify _ x (TyPtrF t1) (TyPtrF t2) = pure (mempty, [With x $ t1 :~ t2])
  liftUnify _ x (TYPEF rep1) (TYPEF rep2) = pure (mempty, [With x $ rep1 :~ rep2])
  liftUnify _ _ TyRepF TyRepF = pure (mempty, [])
  liftUnify _ _ (RepF rep1) (RepF rep2) | rep1 == rep2 = pure (mempty, [])
  liftUnify _ x t1 t2 = errorOn x $ unifyErrorMessage t1 t2
  liftEquiv equiv (TyAppF t11 t12) (TyAppF t21 t22) = (<>) <$> equiv t11 t21 <*> equiv t12 t22
  liftEquiv _ (TyVarF v1) (TyVarF v2) | v1 == v2 = Just mempty
  liftEquiv _ (TyConF c1) (TyConF c2) | c1 == c2 = Just mempty
  liftEquiv _ (TyPrimF p1) (TyPrimF p2) | p1 == p2 = Just mempty
  liftEquiv equiv (TyArrF l1 r1) (TyArrF l2 r2) = (<>) <$> equiv l1 l2 <*> equiv r1 r2
  liftEquiv _ (TyTupleF n1) (TyTupleF n2) | n1 == n2 = Just mempty
  liftEquiv equiv (TyRecordF kts1) (TyRecordF kts2)
    | Map.keys kts1 == Map.keys kts2 = mconcat <$> zipWithM equiv (Map.elems kts1) (Map.elems kts2)
  liftEquiv _ TyLazyF TyLazyF = Just mempty
  liftEquiv equiv (TyPtrF t1) (TyPtrF t2) = equiv t1 t2
  liftEquiv equiv (TYPEF rep1) (TYPEF rep2) = equiv rep1 rep2
  liftEquiv _ TyRepF TyRepF = Just mempty
  liftEquiv _ (RepF rep1) (RepF rep2) | rep1 == rep2 = Just mempty
  liftEquiv _ _ _ = Nothing
  liftFreevars freevars = foldMap freevars
  liftOccursCheck occursCheck v t = or $ fmap (occursCheck v) t

instance (MonadIO m, MonadReader env m, HasOpt env, HasUniqSupply env) => MonadBind (UTerm TypeF TypeVar) (TypeUnifyT m) where
  lookupVar v = view (at v) <$> get
  freshVar = do
    rep <- TypeVar <$> newLocalId "r" (UTerm TyRepF)
    kind <- TypeVar <$> newLocalId "k" (UTerm $ TYPEF $ UVar rep)
    TypeVar <$> newLocalId "t" (UVar kind)
  bindVar x v t = do
    when (occursCheck v t) $ errorOn x $ "Occurs check:" <+> quotes (pPrint v) <+> "for" <+> pPrint t
    tKind <- kindOf t
    let cs = [With x $ v ^. typeVar . idMeta :~ tKind]
    solve cs
    at v ?= t
  zonk (UVar v) = do
    mterm <- lookupVar v
    mterm <- traverse zonk mterm
    pure $ fromMaybe (UVar v) mterm
  zonk (UTerm t) = UTerm <$> traverse zonk t

------------
-- Solver --
------------

solve ::
  HasCallStack =>
  (MonadBind t m, MonadReader env m, MonadIO m, HasOpt env) =>
  [With SourcePos (Constraint t)] ->
  m ()
solve = solveLoop 5000

solveLoop ::
  HasCallStack =>
  (MonadBind t m, MonadReader env m, MonadIO m, HasOpt env) =>
  Int ->
  [With SourcePos (Constraint t)] ->
  m ()
solveLoop n _ | n <= 0 = error "Constraint solver error: iteration limit"
solveLoop _ [] = pure ()
solveLoop n (With x (t1 :~ t2) : cs) = do
  (binds, cs') <- unify x t1 t2
  ifor_ binds $ \var term -> bindVar x var term
  solveLoop (n - 1) =<< traverse zonkConstraint (cs' <> cs)

zonkConstraint :: (MonadBind t f) => With x (Constraint t) -> f (With x (Constraint t))
zonkConstraint (With m (x :~ y)) = With m <$> ((:~) <$> zonk x <*> zonk y)

generalize :: (MonadBind UType m, MonadIO m, MonadReader env m, HasUniqSupply env) => SourcePos -> HashSet TypeVar -> UType -> m (Scheme UType)
generalize x bound term = do
  {-
  let fvs = Set.toList $ unboundFreevars bound term
  as <- zipWithM toBound fvs [[c] | c <- ['a' ..]]
  zipWithM_ (\fv a -> bindVar x fv $ UTerm $ TyVar a) fvs as
  Forall as <$> zonkUTerm term
  -}
  zonkedTerm <- zonk term
  let fvs = List.sort $ HashSet.toList $ unboundFreevars bound zonkedTerm
  as <- zipWithM (toBound x) fvs [[c] | c <- ['a' ..]]
  zipWithM_ (\fv a -> bindVar x fv $ UTerm $ TyVarF a) fvs as
  Forall as <$> zonk zonkedTerm

toBound :: (MonadBind UType m, MonadIO m, MonadReader env m, HasUniqSupply env) => SourcePos -> TypeVar -> [Char] -> m (Id UType)
toBound x tv hint = do
  tvType <- defaultToBoxed x $ tv ^. typeVar . idMeta
  tvKind <- kindOf tvType
  -- TODO: tvが無名ならhintを、ソースコード上に現れるならその名前を優先する
  newLocalId hint tvKind

defaultToBoxed :: (Applicative m, MonadBind UType m) => SourcePos -> UType -> m UType
defaultToBoxed x (UVar v) = do
  vKind <- kindOf $ v ^. typeVar . idMeta
  case vKind of
    UTerm TyRepF -> bindVar x v (UTerm $ RepF BoxedRep) >> pure (UTerm $ RepF BoxedRep)
    _ -> do
      void $ defaultToBoxed x =<< kindOf (v ^. typeVar . idMeta)
      UVar <$> traverseOf (typeVar . idMeta) zonk v
defaultToBoxed x (UTerm t) = do
  t <- defaultToBoxed' t
  pure $ UTerm t
  where
    defaultToBoxed' (TyAppF t1 t2) = do
      t1 <- defaultToBoxed x t1
      t2 <- defaultToBoxed x t2
      pure $ TyAppF t1 t2
    defaultToBoxed' (TyVarF v) = do
      ty <- defaultToBoxed x $ v ^. idMeta
      let v' = set idMeta ty v
      pure $ TyVarF v'
    defaultToBoxed' (TyConF c) = do
      ty <- defaultToBoxed x $ c ^. idMeta
      let c' = set idMeta ty c
      pure $ TyConF c'
    defaultToBoxed' (TyPrimF prim) = pure $ TyPrimF prim
    defaultToBoxed' (TyArrF t1 t2) = do
      t1 <- defaultToBoxed x t1
      t2 <- defaultToBoxed x t2
      pure $ TyArrF t1 t2
    defaultToBoxed' (TyTupleF n) = pure $ TyTupleF n
    defaultToBoxed' (TyRecordF kvs) = do
      kvs <- traverse (defaultToBoxed x) kvs
      pure $ TyRecordF kvs
    defaultToBoxed' TyLazyF = pure TyLazyF
    defaultToBoxed' (TyPtrF t) = do
      t <- defaultToBoxed x t
      pure $ TyPtrF t
    defaultToBoxed' TyBottomF = pure TyBottomF
    defaultToBoxed' (TYPEF rep) = do
      rep <- defaultToBoxed x rep
      pure $ TYPEF rep
    defaultToBoxed' TyRepF = pure TyRepF
    defaultToBoxed' (RepF rep) = pure $ RepF rep

unboundFreevars :: Unifiable t => HashSet (Var t) -> t -> HashSet (Var t)
unboundFreevars bound t = HashSet.difference (freevars t) bound

generalizeMutRecs :: (MonadBind UType m, MonadIO m, MonadReader env m, HasUniqSupply env) => SourcePos -> HashSet TypeVar -> [UType] -> m ([Id UType], [UType])
generalizeMutRecs x bound terms = do
  {-
  let fvs = Set.toList $ mconcat $ map (unboundFreevars bound) terms
  as <- zipWithM toBound fvs [[c] | c <- ['a' ..]]
  zipWithM_ (\fv a -> bindVar x fv $ UTerm $ TyVar a) fvs as
  (as,) <$> traverse zonkUTerm terms
  -}
  zonkedTerms <- traverse zonk terms
  let fvs = List.sort $ HashSet.toList $ mconcat $ map (unboundFreevars bound) zonkedTerms
  as <- zipWithM (toBound x) fvs [[c] | c <- ['a' ..]]
  zipWithM_ (\fv a -> bindVar x fv $ UTerm $ TyVarF a) fvs as
  (as,) <$> traverse zonk zonkedTerms

instantiate :: (MonadBind UType m, MonadReader env m, MonadIO m, HasOpt env) => SourcePos -> Scheme UType -> m UType
instantiate x (Forall as t) = do
  avs <- traverse ?? as $ \a -> do
    v <- UVar <$> freshVar @UType
    vKind <- kindOf v
    solve [With x $ a ^. idMeta :~ vKind]
    pure (a, v)
  pure $ applySubst avs t
