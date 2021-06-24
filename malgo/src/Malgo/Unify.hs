{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Unification
module Malgo.Unify where

import Control.Monad.Except (ExceptT)
import qualified Data.HashSet as HashSet
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Pretty
import Malgo.Prelude
import Malgo.TypeCheck.TcEnv (TcEnv, abbrEnv)
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
data Constraint = UType :~ UType
  deriving stock (Eq, Ord, Show, Generic)

instance Pretty Constraint where
  pPrint (t1 :~ t2) = pPrint t1 <+> "~" <+> pPrint t2

---------------
-- Unifiable --
---------------

type UnifyResult = (HashMap TypeVar UType, [With SourcePos Constraint])

unifyErrorMessage :: (Pretty a, Pretty b) => a -> b -> Doc
unifyErrorMessage t1 t2 = "Couldn't match" $$ nest 7 (pPrint t1) $$ nest 2 ("with" <+> pPrint t2)

class Monad m => MonadBind m where
  lookupVar :: TypeVar -> m (Maybe UType)
  default lookupVar :: (MonadTrans tr, MonadBind m1, m ~ tr m1) => TypeVar -> m (Maybe UType)
  lookupVar v = lift (lookupVar v)
  freshVar :: m TypeVar
  default freshVar :: (MonadTrans tr, MonadBind m1, m ~ tr m1) => m TypeVar
  freshVar = lift freshVar
  bindVar :: SourcePos -> TypeVar -> UType -> m ()
  default bindVar :: (MonadTrans tr, MonadBind m1, m ~ tr m1) => SourcePos -> TypeVar -> UType -> m ()
  bindVar x v t = lift (bindVar x v t)

  -- Apply all substituation
  zonk :: UType -> m UType
  default zonk :: (MonadTrans tr, MonadBind m1, m ~ tr m1) => UType -> m UType
  zonk t = lift (zonk t)

instance MonadBind m => MonadBind (ReaderT r m)

instance MonadBind m => MonadBind (ExceptT e m)

instance MonadBind m => MonadBind (StateT s m)

instance (Monoid w, MonadBind m) => MonadBind (WriterT w m)

unify :: (MonadIO m, MonadReader env m, HasOpt env) => SourcePos -> UType -> UType -> m UnifyResult
unify _ (UVar v1) (UVar v2)
  | v1 == v2 = pure (mempty, [])
  | otherwise = pure (HashMap.singleton v1 (UVar v2), [])
unify _ (UVar v) (UTerm t) = pure (HashMap.singleton v (UTerm t), [])
unify _ (UTerm t) (UVar v) = pure (HashMap.singleton v (UTerm t), [])
unify x (UTerm t1) (UTerm t2) = liftUnify x t1 t2

equiv :: UType -> UType -> Maybe (HashMap TypeVar TypeVar)
equiv (UVar v1) (UVar v2)
  | v1 == v2 = Just mempty
  | otherwise = Just $ HashMap.singleton v1 v2
equiv (UTerm t1) (UTerm t2) = liftEquiv t1 t2
equiv _ _ = Nothing

occursCheck :: TypeVar -> UType -> Bool
occursCheck v t = HashSet.member v (freevars t)

liftUnify :: (HasOpt env, MonadReader env m, MonadIO m) => SourcePos -> TypeF UType -> TypeF UType -> m UnifyResult
liftUnify x (TyAppF t11 t12) (TyAppF t21 t22) = pure (mempty, [With x $ t11 :~ t21, With x $ t12 :~ t22])
liftUnify _ (TyVarF v1) (TyVarF v2) | v1 == v2 = pure (mempty, [])
liftUnify _ (TyConF c1) (TyConF c2) | c1 == c2 = pure (mempty, [])
liftUnify _ (TyPrimF p1) (TyPrimF p2) | p1 == p2 = pure (mempty, [])
liftUnify x (TyArrF l1 r1) (TyArrF l2 r2) = pure (mempty, [With x $ l1 :~ l2, With x $ r1 :~ r2])
liftUnify _ (TyTupleF n1) (TyTupleF n2) | n1 == n2 = pure (mempty, [])
liftUnify x (TyRecordF kts1) (TyRecordF kts2)
  | Map.keys kts1 == Map.keys kts2 = pure (mempty, zipWith (\t1 t2 -> With x $ t1 :~ t2) (Map.elems kts1) (Map.elems kts2))
liftUnify _ TyLazyF TyLazyF = pure (mempty, [])
liftUnify x (TyPtrF t1) (TyPtrF t2) = pure (mempty, [With x $ t1 :~ t2])
liftUnify x (TYPEF rep1) (TYPEF rep2) = pure (mempty, [With x $ rep1 :~ rep2])
liftUnify _ TyRepF TyRepF = pure (mempty, [])
liftUnify _ (RepF rep1) (RepF rep2) | rep1 == rep2 = pure (mempty, [])
liftUnify x t1 t2 = errorOn x $ unifyErrorMessage t1 t2

liftEquiv :: TypeF UType -> TypeF UType -> Maybe (HashMap TypeVar TypeVar)
liftEquiv (TyAppF t11 t12) (TyAppF t21 t22) = (<>) <$> equiv t11 t21 <*> equiv t12 t22
liftEquiv (TyVarF v1) (TyVarF v2) | v1 == v2 = Just mempty
liftEquiv (TyConF c1) (TyConF c2) | c1 == c2 = Just mempty
liftEquiv (TyPrimF p1) (TyPrimF p2) | p1 == p2 = Just mempty
liftEquiv (TyArrF l1 r1) (TyArrF l2 r2) = (<>) <$> equiv l1 l2 <*> equiv r1 r2
liftEquiv (TyTupleF n1) (TyTupleF n2) | n1 == n2 = Just mempty
liftEquiv (TyRecordF kts1) (TyRecordF kts2)
  | Map.keys kts1 == Map.keys kts2 = mconcat <$> zipWithM equiv (Map.elems kts1) (Map.elems kts2)
liftEquiv TyLazyF TyLazyF = Just mempty
liftEquiv (TyPtrF t1) (TyPtrF t2) = equiv t1 t2
liftEquiv (TYPEF rep1) (TYPEF rep2) = equiv rep1 rep2
liftEquiv TyRepF TyRepF = Just mempty
liftEquiv (RepF rep1) (RepF rep2) | rep1 == rep2 = Just mempty
liftEquiv _ _ = Nothing

liftOccursCheck :: TypeVar -> TypeF UType -> Bool
liftOccursCheck v t = or $ fmap (occursCheck v) t

instance (MonadReader env m, HasUniqSupply env, HasOpt env, MonadIO m, MonadState TcEnv m) => MonadBind (TypeUnifyT m) where
  lookupVar v = view (at v) <$> TypeUnifyT get

  freshVar = do
    rep <- TypeVar <$> newLocalId "r" (UTerm TyRepF)
    kind <- TypeVar <$> newLocalId "k" (UTerm $ TYPEF $ UVar rep)
    TypeVar <$> newLocalId "t" (UVar kind)

  bindVar x v t = do
    when (occursCheck v t) $ errorOn x $ "Occurs check:" <+> quotes (pPrint v) <+> "for" <+> pPrint t
    tKind <- kindOf t
    let cs = [With x $ v ^. typeVar . idMeta :~ tKind]
    solve cs
    TypeUnifyT $ at v ?= t

  zonk (UVar v) = do
    mterm <- lookupVar v
    mterm <- traverse zonk mterm
    pure $ fromMaybe (UVar v) mterm
  zonk (UTerm t) = UTerm <$> traverse zonk t

------------
-- Solver --
------------

solve :: (MonadIO f, MonadReader env f, HasOpt env, MonadBind f, MonadState TcEnv f) => [With SourcePos Constraint] -> f ()
solve = solveLoop 5000

solveLoop :: (MonadIO f, MonadReader env f, HasOpt env, MonadBind f, MonadState TcEnv f) => Int -> [With SourcePos Constraint] -> f ()
solveLoop n _ | n <= 0 = error "Constraint solver error: iteration limit"
solveLoop _ [] = pure ()
solveLoop n (With x (t1 :~ t2) : cs) = do
  abbrEnv <- use abbrEnv
  let t1' = fromMaybe t1 (expandTypeSynonym abbrEnv t1)
  let t2' = fromMaybe t2 (expandTypeSynonym abbrEnv t2)
  (binds, cs') <- unify x t1' t2'
  ifor_ binds $ \var term -> bindVar x var term
  solveLoop (n - 1) =<< traverse zonkConstraint (cs' <> cs)

zonkConstraint :: MonadBind f => With x Constraint -> f (With x Constraint)
zonkConstraint (With m (x :~ y)) = With m <$> ((:~) <$> zonk x <*> zonk y)

generalize :: (MonadBind m, MonadIO m, HasUniqSupply env, MonadReader env m) => SourcePos -> HashSet TypeVar -> UType -> m (Scheme UType)
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

toBound :: (MonadBind m, MonadIO m, HasUniqSupply env, MonadReader env m) => SourcePos -> TypeVar -> String -> m (Id UType)
toBound x tv hint = do
  tvType <- defaultToBoxed x $ tv ^. typeVar . idMeta
  tvKind <- kindOf tvType
  -- TODO: tvが無名ならhintを、ソースコード上に現れるならその名前を優先する
  newLocalId hint tvKind

defaultToBoxed :: MonadBind f => SourcePos -> UType -> f UType
defaultToBoxed x t = transformM ?? t $ \case
  UVar v -> do
    vKind <- kindOf $ v ^. typeVar . idMeta
    case vKind of
      UTerm TyRepF -> bindVar x v (UTerm $ RepF BoxedRep) >> pure (UTerm $ RepF BoxedRep)
      _ -> do
        void $ defaultToBoxed x =<< kindOf (v ^. typeVar . idMeta)
        UVar <$> traverseOf (typeVar . idMeta) zonk v
  UTerm t -> pure $ UTerm t

unboundFreevars :: HashSet TypeVar -> UType -> HashSet TypeVar
unboundFreevars bound t = HashSet.difference (freevars t) bound

generalizeMutRecs :: (MonadBind m, MonadIO m, HasUniqSupply env, MonadReader env m) => SourcePos -> HashSet TypeVar -> [UType] -> m ([Id UType], [UType])
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

instantiate :: (MonadBind m, MonadIO m, MonadReader env m, HasOpt env, MonadState TcEnv m) => SourcePos -> Scheme UType -> m UType
instantiate x (Forall as t) = do
  avs <- traverse ?? as $ \a -> do
    v <- UVar <$> freshVar
    vKind <- kindOf v
    solve [With x $ a ^. idMeta :~ vKind]
    pure (a, v)
  pure $ applySubst avs t
