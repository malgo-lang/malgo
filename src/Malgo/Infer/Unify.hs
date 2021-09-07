{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Unification
module Malgo.Infer.Unify where

import Control.Lens (At (at), itraverse_, transformM, traverseOf, use, view, (?=), (^.))
import Control.Monad.Writer.Strict (WriterT)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Traversable (for)
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Pretty
import Malgo.Infer.TcEnv (TcEnv, abbrEnv)
import Malgo.Infer.UTerm
import Malgo.Prelude hiding (Constraint)
import Malgo.TypeRep.Static (Rep (..), Scheme (..))
import Malgo.TypeRep.UTerm
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

type UnifyResult = Either (SourcePos, Doc) (HashMap TypeVar UType, [With SourcePos Constraint])

unifyErrorMessage :: (Pretty a, Pretty b) => a -> b -> Doc
unifyErrorMessage t1 t2 = "Couldn't match" $$ nest 7 (pPrint t1) $$ nest 2 ("with" <+> pPrint t2)

unify :: SourcePos -> UType -> UType -> UnifyResult
unify _ (UVar v1) (UVar v2)
  | v1 == v2 = pure (mempty, [])
  | otherwise = pure (HashMap.singleton v1 (UVar v2), [])
unify _ (UVar v) t = pure (HashMap.singleton v t, [])
unify _ t (UVar v) = pure (HashMap.singleton v t, [])
unify x (TyApp t11 t12) (TyApp t21 t22) = pure (mempty, [With x $ t11 :~ t21, With x $ t12 :~ t22])
unify _ (TyVar v1) (TyVar v2) | v1 == v2 = pure (mempty, [])
unify _ (TyCon c1) (TyCon c2) | c1 == c2 = pure (mempty, [])
unify _ (TyPrim p1) (TyPrim p2) | p1 == p2 = pure (mempty, [])
unify x (TyArr l1 r1) (TyArr l2 r2) = pure (mempty, [With x $ l1 :~ l2, With x $ r1 :~ r2])
unify _ (TyTuple n1) (TyTuple n2) | n1 == n2 = pure (mempty, [])
unify x (TyRecord kts1) (TyRecord kts2)
  | Map.keys kts1 == Map.keys kts2 = pure (mempty, zipWith (\t1 t2 -> With x $ t1 :~ t2) (Map.elems kts1) (Map.elems kts2))
unify _ TyLazy TyLazy = pure (mempty, [])
unify x (TyPtr t1) (TyPtr t2) = pure (mempty, [With x $ t1 :~ t2])
unify x (TYPE rep1) (TYPE rep2) = pure (mempty, [With x $ rep1 :~ rep2])
unify _ TyRep TyRep = pure (mempty, [])
unify _ (Rep rep1) (Rep rep2) | rep1 == rep2 = pure (mempty, [])
unify x t1 t2 = Left (x, unifyErrorMessage t1 t2)

equiv :: UType -> UType -> Maybe (HashMap TypeVar TypeVar)
equiv (UVar v1) (UVar v2)
  | v1 == v2 = Just mempty
  | otherwise = Just $ HashMap.singleton v1 v2
equiv (TyApp t11 t12) (TyApp t21 t22) = (<>) <$> equiv t11 t21 <*> equiv t12 t22
equiv (TyVar v1) (TyVar v2) | v1 == v2 = Just mempty
equiv (TyCon c1) (TyCon c2) | c1 == c2 = Just mempty
equiv (TyPrim p1) (TyPrim p2) | p1 == p2 = Just mempty
equiv (TyArr l1 r1) (TyArr l2 r2) = (<>) <$> equiv l1 l2 <*> equiv r1 r2
equiv (TyTuple n1) (TyTuple n2) | n1 == n2 = Just mempty
equiv (TyRecord kts1) (TyRecord kts2) | Map.keys kts1 == Map.keys kts2 = mconcat <$> zipWithM equiv (Map.elems kts1) (Map.elems kts2)
equiv TyLazy TyLazy = Just mempty
equiv (TyPtr t1) (TyPtr t2) = equiv t1 t2
equiv (TYPE rep1) (TYPE rep2) = equiv rep1 rep2
equiv TyRep TyRep = Just mempty
equiv (Rep rep1) (Rep rep2) | rep1 == rep2 = Just mempty
equiv _ _ = Nothing

occursCheck :: TypeVar -> UType -> Bool
occursCheck v t = HashSet.member v (freevars t)

instance (MonadReader env m, HasUniqSupply env, HasOpt env, MonadIO m, MonadState TcEnv m) => MonadBind (TypeUnifyT m) where
  lookupVar v = view (at v) <$> TypeUnifyT get

  freshVar = do
    rep <- TypeVar <$> newInternalId "r" TyRep
    kind <- TypeVar <$> newInternalId "k" (TYPE $ UVar rep)
    TypeVar <$> newInternalId "t" (UVar kind)

  bindVar x v t = do
    when (occursCheck v t) $ errorOn x $ "Occurs check:" <+> quotes (pPrint v) <+> "for" <+> pPrint t
    solve [With x $ v ^. typeVar . idMeta :~ kindOf t]
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
  case unify x t1' t2' of
    Left (pos, message) -> errorOn pos message
    Right (binds, cs') -> do
      itraverse_ (bindVar x) binds
      solveLoop (n - 1) =<< traverse zonkConstraint (cs' <> cs)

zonkConstraint :: MonadBind f => With x Constraint -> f (With x Constraint)
zonkConstraint (With m (x :~ y)) = With m <$> ((:~) <$> zonk x <*> zonk y)

generalize :: (MonadBind m, MonadIO m, HasUniqSupply env, MonadReader env m) => SourcePos -> HashSet TypeVar -> UType -> m (Scheme UType)
generalize x bound term = do
  zonkedTerm <- zonk term
  let fvs = HashSet.toList $ unboundFreevars bound zonkedTerm
  as <- zipWithM (toBound x) fvs [Text.singleton c | c <- ['a' ..]]
  zipWithM_ (\fv a -> bindVar x fv $ TyVar a) fvs as
  Forall as <$> zonk zonkedTerm

toBound :: (MonadBind m, MonadIO m, HasUniqSupply env, MonadReader env m) => SourcePos -> TypeVar -> Text -> m (Id UType)
toBound x tv hint = do
  tvType <- defaultToBoxed x $ tv ^. typeVar . idMeta
  let tvKind = kindOf tvType
  let name = case tv ^. typeVar . idName of
        x
          | x == noName -> hint
          | otherwise -> x
  newInternalId name tvKind

defaultToBoxed :: MonadBind f => SourcePos -> UType -> f UType
defaultToBoxed x t = transformM ?? t $ \case
  UVar v -> do
    let vKind = kindOf $ v ^. typeVar . idMeta
    case vKind of
      TyRep -> bindVar x v (Rep BoxedRep) >> pure (Rep BoxedRep)
      _ -> do
        void $ defaultToBoxed x vKind
        UVar <$> traverseOf (typeVar . idMeta) zonk v
  UTerm t -> pure $ UTerm t

unboundFreevars :: HashSet TypeVar -> UType -> HashSet TypeVar
unboundFreevars bound t = HashSet.difference (freevars t) bound

generalizeMutRecs :: (MonadBind m, MonadIO m, HasUniqSupply env, MonadReader env m) => SourcePos -> HashSet TypeVar -> [UType] -> m ([Id UType], [UType])
generalizeMutRecs x bound terms = do
  zonkedTerms <- traverse zonk terms
  let fvs = HashSet.toList $ mconcat $ map (unboundFreevars bound) zonkedTerms
  as <- zipWithM (toBound x) fvs [Text.singleton c | c <- ['a' ..]]
  zipWithM_ (\fv a -> bindVar x fv $ TyVar a) fvs as
  (as,) <$> traverse zonk zonkedTerms

instantiate :: (MonadBind m, MonadIO m, MonadReader env m, HasOpt env, MonadState TcEnv m) => SourcePos -> Scheme UType -> m UType
instantiate x (Forall as t) = do
  avs <- for as \a -> do
    v <- UVar <$> freshVar
    solve [With x $ a ^. idMeta :~ kindOf v]
    pure (a, v)
  pure $ applySubst avs t
