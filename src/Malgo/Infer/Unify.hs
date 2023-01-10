{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

-- | Unification
module Malgo.Infer.Unify where

import Control.Lens (At (at), itraverse_, use, view, (%=), (<>=), (?=))
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Traversable (for)
import Koriel.Id
import Koriel.Lens
import Koriel.MonadUniq
import Koriel.Pretty
import Malgo.Infer.TcEnv (TcEnv)
import Malgo.Infer.TypeRep
import Malgo.Prelude hiding (Constraint)
import Malgo.Rename.RnEnv (RnEnv)

-- * Constraint

infixl 5 :~

-- | Constraint
-- a :~ b means 'a ~ b'
data Constraint = Type :~ Type
  deriving stock (Eq, Ord, Show, Generic)

instance Pretty Constraint where
  pPrint (t1 :~ t2) = pPrint t1 <+> "~" <+> pPrint t2

-- * Unifiable

-- | Monad that handles substitution over type variables
class Monad m => MonadBind m where
  lookupVar :: MetaVar -> m (Maybe Type)
  default lookupVar :: (MonadTrans tr, MonadBind m1, m ~ tr m1) => MetaVar -> m (Maybe Type)
  lookupVar v = lift (lookupVar v)
  freshVar :: Maybe Text -> m MetaVar
  default freshVar :: (MonadTrans tr, MonadBind m1, m ~ tr m1) => Maybe Text -> m MetaVar
  freshVar = lift . freshVar
  bindVar :: HasCallStack => Range -> MetaVar -> Type -> m ()
  default bindVar :: (MonadTrans tr, MonadBind m1, m ~ tr m1) => Range -> MetaVar -> Type -> m ()
  bindVar x v t = lift (bindVar x v t)

  -- | Apply all substituation
  zonk :: Type -> m Type
  default zonk :: (MonadTrans tr, MonadBind m1, m ~ tr m1) => Type -> m Type
  zonk t = lift (zonk t)

instance MonadBind m => MonadBind (ReaderT r m)

instance MonadBind m => MonadBind (ExceptT e m)

instance MonadBind m => MonadBind (StateT s m)

instance (Monoid w, MonadBind m) => MonadBind (WriterT w m)

-- | 'Right' (substituation, new constraints) or 'Left' (position, error message)
type UnifyResult = Either (Range, Doc) (HashMap MetaVar Type, [(Range, Constraint)])

-- | Unify two types
unify :: Range -> Type -> Type -> UnifyResult
unify _ (TyMeta v1) (TyMeta v2)
  | v1 == v2 = pure (mempty, [])
  | otherwise = pure (one (v1, TyMeta v2), [])
unify _ (TyMeta v) t = pure (one (v, t), [])
unify _ t (TyMeta v) = pure (one (v, t), [])
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
    unifyErrorMessage t1 t2 = "Couldn't match" $$ nest 7 (pPrint t1) $$ nest 2 ("with" <+> pPrint t2)

instance (MonadReader env m, HasUniqSupply env UniqSupply, MonadIO m, MonadState TcEnv m, HasModuleName env ModuleName) => MonadBind (TypeUnifyT m) where
  lookupVar v = view (at v) <$> TypeUnifyT get

  freshVar hint = do
    hint <- pure $ fromMaybe "t" hint
    kind <- newTemporalId ("k" <> hint) ()
    kindCtx <>= one (kind, TYPE)
    newVar <- newInternalId hint ()
    kindCtx <>= one (newVar, TyMeta $ MetaVar kind)
    pure $ MetaVar newVar

  bindVar x v t = do
    ctx <- use kindCtx
    when (occursCheck ctx v t) $ errorOn x $ "Occurs check:" <+> quotes (pPrint v) <+> "for" <+> pPrint t
    solve [(x, kindOf ctx v.metaVar :~ kindOf ctx t)]
    TypeUnifyT $ at v ?= t
    where
      occursCheck :: KindCtx -> MetaVar -> Type -> Bool
      occursCheck ctx v t = HashSet.member v (freevars ctx t)

  zonk (TyApp t1 t2) = TyApp <$> zonk t1 <*> zonk t2
  zonk (TyVar v) = do
    ctx <- use kindCtx
    k <- zonk $ kindOf ctx v
    kindCtx %= HashMap.insert v k
    pure $ TyVar v
  zonk (TyCon c) = do
    ctx <- use kindCtx
    k <- zonk $ kindOf ctx c
    kindCtx %= HashMap.insert c k
    pure $ TyCon c
  zonk t@TyPrim {} = pure t
  zonk (TyArr t1 t2) = TyArr <$> zonk t1 <*> zonk t2
  zonk t@TyTuple {} = pure t
  zonk (TyRecord kts) = TyRecord <$> traverse zonk kts
  zonk TyPtr = pure TyPtr
  zonk TYPE = pure TYPE
  zonk t@(TyMeta v) = fromMaybe t <$> (traverse zonk =<< lookupVar v)

-- Anothor implementation using `Plated`
-- zonk =
--   transformM $ \case
--     TyMeta v -> fromMaybe (TyMeta v) <$> (traverse zonk =<< lookupVar v)
--     ty -> pure ty

-- * Constraint solver

solve :: (MonadIO f, MonadBind f, MonadState TcEnv f) => [(Range, Constraint)] -> f ()
solve = solveLoop (5000 :: Int)
  where
    solveLoop n _ | n <= 0 = error "Constraint solver error: iteration limit"
    solveLoop _ [] = pass
    solveLoop n ((x, t1 :~ t2) : cs) = do
      abbrEnv <- use typeSynonymMap
      let t1' = fromMaybe t1 (expandTypeSynonym abbrEnv t1)
      let t2' = fromMaybe t2 (expandTypeSynonym abbrEnv t2)
      case unify x t1' t2' of
        Left (pos, message) -> errorOn pos message
        Right (binds, cs') -> do
          itraverse_ (bindVar x) binds
          constraints <- traverse zonkConstraint (cs' <> cs)
          solveLoop (n - 1) constraints
    zonkConstraint (m, x :~ y) = (m,) <$> ((:~) <$> zonk x <*> zonk y)

generalize :: (MonadBind m, MonadIO m, MonadReader RnEnv m, MonadState TcEnv m) => Range -> HashSet MetaVar -> Type -> m (Scheme Type)
generalize x bound term = do
  zonkedTerm <- zonk term
  ctx <- use kindCtx
  let fvs = HashSet.toList $ unboundFreevars ctx bound zonkedTerm
  as <- traverse (toBound x) fvs
  zipWithM_ (\fv a -> bindVar x fv $ TyVar a) fvs as
  Forall as <$> zonk zonkedTerm

generalizeMutRecs :: (MonadBind m, MonadIO m, MonadReader RnEnv m, MonadState TcEnv m) => Range -> HashSet MetaVar -> [Type] -> m ([TypeVar], [Type])
generalizeMutRecs x bound terms = do
  zonkedTerms <- traverse zonk terms
  ctx <- use kindCtx
  let fvs = HashSet.toList $ mconcat $ map (unboundFreevars ctx bound) zonkedTerms
  as <- traverse (toBound x) fvs
  zipWithM_ (\fv a -> bindVar x fv $ TyVar a) fvs as
  (as,) <$> traverse zonk zonkedTerms

toBound :: (MonadBind m, MonadIO m, MonadReader RnEnv m, MonadState TcEnv m) => Range -> MetaVar -> m (Id ())
toBound x tv = do
  ctx <- use kindCtx
  tvType <- defaultToBoxed x $ kindOf ctx tv.metaVar
  ctx <- use kindCtx
  let tvKind = kindOf ctx tvType
  let name = tv.metaVar.name
  boundVar <- newInternalId name ()
  kindCtx %= HashMap.insert boundVar tvKind
  pure boundVar

-- TODO: Rewrite this function as a modifier of `kindCtx`.
defaultToBoxed :: (MonadBind f, MonadState TcEnv f) => Range -> Type -> f Type
defaultToBoxed x = \case
  TyApp ty ty' -> TyApp <$> defaultToBoxed x ty <*> defaultToBoxed x ty'
  TyVar id -> do
    ctx <- use kindCtx
    k <- defaultToBoxed x $ kindOf ctx id
    kindCtx %= HashMap.insert id k
    pure $ TyVar id
  TyCon id -> do
    ctx <- use kindCtx
    k <- defaultToBoxed x $ kindOf ctx id
    kindCtx %= HashMap.insert id k
    pure $ TyCon id
  TyPrim pt -> pure $ TyPrim pt
  TyArr ty ty' -> TyArr <$> defaultToBoxed x ty <*> defaultToBoxed x ty'
  TyTuple n -> pure $ TyTuple n
  TyRecord hm -> TyRecord <$> traverse (defaultToBoxed x) hm
  TyPtr -> pure TyPtr
  TYPE -> pure TYPE
  TyMeta tv -> do
    ctx <- use kindCtx
    let vKind = kindOf ctx tv.metaVar
    void $ defaultToBoxed x vKind
    ctx <- use kindCtx
    k <- zonk $ kindOf ctx tv.metaVar
    kindCtx %= HashMap.insert tv.metaVar k
    pure $ TyMeta tv

-- TODO: lift to a monadic action
unboundFreevars :: KindCtx -> HashSet MetaVar -> Type -> HashSet MetaVar
unboundFreevars ctx bound t = HashSet.difference (freevars ctx t) bound

instantiate :: (MonadBind m, MonadIO m, MonadState TcEnv m) => Range -> Scheme Type -> m Type
instantiate x (Forall as t) = do
  avs <- for as \a -> do
    v <- TyMeta <$> freshVar (Just $ a.name)
    ctx <- use kindCtx
    solve [(x, kindOf ctx a :~ kindOf ctx v)]
    pure (a, v)
  pure $ applySubst (HashMap.fromList avs) t
