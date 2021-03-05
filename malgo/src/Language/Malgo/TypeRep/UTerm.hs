{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.TypeRep.UTerm where

import Data.Deriving
import Data.Fix
import qualified Data.HashSet as HashSet
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Void
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Pretty
import Language.Malgo.Prelude
import Language.Malgo.TypeRep.Static (IsScheme, IsType (safeToType), PrimT (..), Rep (..))
import qualified Language.Malgo.TypeRep.Static as S
import Language.Malgo.Unify

----------
-- Type --
----------

-- | Definition of Type
data TypeF a
  = TyApp a a
  | TyVar (Id a)
  | TyCon (Id a)
  | TyPrim PrimT
  | TyArr a a
  | TyTuple [a]
  | TyLazy a
  | TyPtr a
  | TYPE a
  | TyRep
  | Rep Rep
  deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

deriveEq1 ''TypeF
deriveOrd1 ''TypeF
deriveShow1 ''TypeF

type UType = UTerm TypeF TypeVar

type Type = Fix TypeF

instance Pretty1 TypeF where
  liftPPrintPrec ppr l d (TyApp t1 t2) =
    maybeParens (d > 10) $ sep [ppr l 10 t1, ppr l 11 t2]
  liftPPrintPrec ppr l d (TyVar v) = liftPPrintPrec ppr l d v
  liftPPrintPrec ppr l d (TyCon c) = liftPPrintPrec ppr l d c
  liftPPrintPrec _ _ _ (TyPrim p) = pPrint p
  liftPPrintPrec ppr l d (TyArr t1 t2) =
    maybeParens (d > 10) $ ppr l 11 t1 <+> "->" <+> ppr l 10 t2
  liftPPrintPrec ppr l _ (TyTuple ts) = parens $ sep $ punctuate "," $ map (ppr l 0) ts
  liftPPrintPrec ppr l _ (TyLazy t) = braces $ ppr l 0 t
  liftPPrintPrec ppr l d (TyPtr t) = maybeParens (d > 10) $ sep ["Ptr#", ppr l 11 t]
  liftPPrintPrec ppr l _ (TYPE rep) = "TYPE" <+> ppr l 0 rep
  liftPPrintPrec _ _ _ TyRep = "#Rep"
  liftPPrintPrec _ l _ (Rep rep) = pPrintPrec l 0 rep

instance Pretty a => Pretty (TypeF a) where
  pPrintPrec l d t = liftPPrintPrec pPrintPrec l d t

instance (IsType a) => IsType (TypeF a) where
  safeToType (TyApp t1 t2) = S.TyApp <$> S.safeToType t1 <*> S.safeToType t2
  safeToType (TyVar v) = S.TyVar <$> traverseOf idMeta S.safeToType v
  safeToType (TyCon c) = S.TyCon <$> traverseOf idMeta S.safeToType c
  safeToType (TyPrim p) = Just $ S.TyPrim p
  safeToType (TyArr t1 t2) = S.TyArr <$> S.safeToType t1 <*> S.safeToType t2
  safeToType (TyTuple ts) = S.TyTuple <$> traverse S.safeToType ts
  safeToType (TyLazy t) = S.TyLazy <$> S.safeToType t
  safeToType (TyPtr t) = S.TyPtr <$> S.safeToType t
  safeToType (TYPE rep) = S.TYPE <$> S.safeToType rep
  safeToType TyRep = Just S.TyRep
  safeToType (Rep rep) = Just $ S.Rep rep
  fromType (S.TyApp t1 t2) = TyApp (S.fromType t1) (S.fromType t2)
  fromType (S.TyVar v) = TyVar (over idMeta (\k -> k ^. re S._Type) v)
  fromType (S.TyCon c) = TyCon (over idMeta (\k -> k ^. re S._Type) c)
  fromType (S.TyPrim p) = TyPrim p
  fromType (S.TyArr t1 t2) = TyArr (S.fromType t1) (S.fromType t2)
  fromType (S.TyTuple ts) = TyTuple (map S.fromType ts)
  fromType (S.TyLazy t) = TyLazy (S.fromType t)
  fromType (S.TyPtr t) = TyPtr (S.fromType t)
  fromType (S.TYPE rep) = TYPE $ S.fromType rep
  fromType S.TyRep = TyRep
  fromType (S.Rep rep) = Rep rep

instance IsType a => S.HasType (TypeF a) where
  typeOf = S.typeOf . S.toType

newtype TypeVar = TypeVar {_typeVar :: Id UType}
  deriving newtype (Eq, Ord, Show, Generic, Hashable)

makeLenses ''TypeVar

instance HasUTerm TypeF TypeVar TypeVar where
  walkOn f (TypeVar x) = TypeVar <$> traverseOf idMeta f x

instance Pretty TypeVar where
  pPrint (TypeVar v) = "'" <> pPrint v

instance Unifiable TypeF TypeVar where
  unify x (TyApp t11 t12) (TyApp t21 t22) = [With x (t11 :~ t21), With x (t12 :~ t22)]
  unify x (TyVar v1) (TyVar v2)
    | v1 == v2 = []
    | otherwise = errorWithMeta x $ unifyErrorMessage v1 v2
  unify x (TyCon c1) (TyCon c2)
    | c1 == c2 = []
    | otherwise = errorWithMeta x $ unifyErrorMessage c1 c2
  unify x (TyPrim p1) (TyPrim p2)
    | p1 == p2 = []
    | otherwise = errorWithMeta x $ unifyErrorMessage p1 p2
  unify x (TyArr l1 r1) (TyArr l2 r2) = [With x (l1 :~ l2), With x (r1 :~ r2)]
  unify x (TyTuple ts1) (TyTuple ts2) = map (With x) $ zipWith (:~) ts1 ts2
  unify x (TyLazy t1) (TyLazy t2) = [With x (t1 :~ t2)]
  unify x (TyPtr t1) (TyPtr t2) = [With x (t1 :~ t2)]
  unify x (TYPE rep1) (TYPE rep2) = [With x (rep1 :~ rep2)]
  unify _ TyRep TyRep = []
  unify _ (Rep rep1) (Rep rep2) | rep1 == rep2 = []
  unify x t1 t2 = errorWithMeta x $ unifyErrorMessage t1 t2

type TypeMap = HashMap TypeVar UType

newtype TypeUnifyT m a = TypeUnifyT {unTypeUnifyT :: StateT TypeMap m a}
  deriving newtype (Functor, Applicative, Monad, MonadState TypeMap, MonadUniq, MonadMalgo, MonadIO)

instance MonadTrans TypeUnifyT where
  lift m = TypeUnifyT $ lift m

runTypeUnifyT :: Monad m => TypeUnifyT m a -> m a
runTypeUnifyT (TypeUnifyT m) = evalStateT m mempty

instance (Monad m, MonadUniq m, MonadIO m) => MonadBind TypeF TypeVar (TypeUnifyT m) where
  lookupVar v = view (at v) <$> get
  freshVar = do
    kind <- TypeVar <$> newLocalId "k" (UTerm TyRep)
    TypeVar <$> newLocalId "t" (UTerm $ TYPE $ UVar kind)
  bindVar x v t = do
    occursCheck x v t
    let cs = [With x $ v ^. typeVar . idMeta :~ (typeOf t `asTypeOf` t)]
    solve cs
    at v ?= t

data Scheme = Forall [Id Type] (UTerm TypeF TypeVar)
  deriving stock (Eq, Ord, Show, Generic)

instance Pretty Scheme where
  pPrintPrec l _ (Forall vs t) = "forall" <+> sep (map (pPrintPrec l 0) vs) <> "." <+> pPrintPrec l 0 t

instance HasUTerm TypeF TypeVar Scheme where
  walkOn f (Forall vs t) = Forall vs <$> walkOn f t

instance IsScheme Scheme where
  safeToScheme (Forall vs t) = do
    let vs' = map (over idMeta S.toType) vs
    t' <- safeToType =<< freeze t
    Just $ S.Forall vs' t'
  fromScheme (S.Forall vs t) = Forall (map (over idMeta S.fromType) vs) (unfreeze $ S.fromType t)

generalize :: (MonadUniq m, MonadBind TypeF TypeVar m, Pretty x) => x -> HashSet TypeVar -> UTerm TypeF TypeVar -> m Scheme
generalize x bound term = do
  {-
  let fvs = Set.toList $ unboundFreevars bound term
  as <- zipWithM toBound fvs [[c] | c <- ['a' ..]]
  zipWithM_ (\fv a -> bindVar x fv $ UTerm $ TyVar a) fvs as
  Forall as <$> zonkUTerm term
  -}
  zonkedTerm <- zonkUTerm term
  let fvs = HashSet.toList $ unboundFreevars bound zonkedTerm
  as <- zipWithM (toBound x) fvs [[c] | c <- ['a' ..]]
  zipWithM_ (\fv a -> bindVar x fv $ UTerm $ TyVar a) fvs $ map (over idMeta unfreeze) as
  Forall as <$> zonkUTerm zonkedTerm

toBound :: (MonadUniq m, MonadBind TypeF TypeVar m, Pretty x) => x -> TypeVar -> [Char] -> m (Id Type)
toBound x tv hint = do
  tvType <- defaultToBoxed x $ tv ^. typeVar . idMeta
  case freeze $ typeOf tvType `asTypeOf` tvType of
    Just kind -> newLocalId hint kind
    Nothing -> errorDoc $ pPrint tvType

defaultToBoxed :: (Applicative m, MonadBind TypeF TypeVar m, Pretty x) => x -> UType -> m UType
defaultToBoxed x (UVar v)
  | typeOf @(UTerm _ TypeVar) (v ^. typeVar . idMeta) == UTerm TyRep = do
    bindVar x v (UTerm $ Rep BoxedRep)
    pure (UTerm $ Rep BoxedRep)
  | otherwise = do
    _ <- defaultToBoxed x (typeOf $ v ^. typeVar . idMeta)
    UVar <$> traverseOf (typeVar . idMeta) zonkUTerm v
defaultToBoxed x (UTerm t) = do
  t <- defaultToBoxed' t
  pure $ UTerm t
  where
    defaultToBoxed' (TyApp t1 t2) = do
      t1 <- defaultToBoxed x t1
      t2 <- defaultToBoxed x t2
      pure $ TyApp t1 t2
    defaultToBoxed' (TyVar v) = do
      ty <- defaultToBoxed x $ v ^. idMeta
      let v' = set idMeta ty v
      pure $ TyVar v'
    defaultToBoxed' (TyCon c) = do
      ty <- defaultToBoxed x $ c ^. idMeta
      let c' = set idMeta ty c
      pure $ TyCon c'
    defaultToBoxed' (TyPrim prim) = pure $ TyPrim prim
    defaultToBoxed' (TyArr t1 t2) = do
      t1 <- defaultToBoxed x t1
      t2 <- defaultToBoxed x t2
      pure $ TyArr t1 t2
    defaultToBoxed' (TyTuple ts) = do
      ts <- traverse (defaultToBoxed x) ts
      pure $ TyTuple ts
    defaultToBoxed' (TyLazy t) = do
      t <- defaultToBoxed x t
      pure $ TyLazy t
    defaultToBoxed' (TyPtr t) = do
      t <- defaultToBoxed x t
      pure $ TyPtr t
    defaultToBoxed' (TYPE rep) = do
      rep <- defaultToBoxed x rep
      pure $ TYPE rep
    defaultToBoxed' TyRep = pure TyRep
    defaultToBoxed' (Rep rep) = pure $ Rep rep

unboundFreevars :: (Eq v, Foldable t, Hashable v) => HashSet v -> UTerm t v -> HashSet v
unboundFreevars bound t = HashSet.difference (freevars t) bound

generalizeMutRecs :: (MonadUniq m, MonadBind TypeF TypeVar m, Pretty x) => x -> HashSet TypeVar -> [UTerm TypeF TypeVar] -> m ([Id Type], [UTerm TypeF TypeVar])
generalizeMutRecs x bound terms = do
  {-
  let fvs = Set.toList $ mconcat $ map (unboundFreevars bound) terms
  as <- zipWithM toBound fvs [[c] | c <- ['a' ..]]
  zipWithM_ (\fv a -> bindVar x fv $ UTerm $ TyVar a) fvs as
  (as,) <$> traverse zonkUTerm terms
  -}
  zonkedTerms <- traverse zonkUTerm terms
  let fvs = HashSet.toList $ mconcat $ map (unboundFreevars bound) zonkedTerms
  as <- zipWithM (toBound x) fvs [[c] | c <- ['a' ..]]
  zipWithM_ (\fv a -> bindVar x fv $ UTerm $ TyVar a) fvs $ map (over idMeta unfreeze) as
  (as,) <$> traverse zonkUTerm zonkedTerms

instantiate :: (MonadBind TypeF TypeVar m) => Scheme -> m (UTerm TypeF TypeVar)
instantiate (Forall as t) = do
  avs <- traverse ?? as $ \a -> (over idMeta unfreeze a,) . UVar <$> freshVar
  replace avs t
  where
    replace _ t@UVar {} = pure t
    replace kvs (UTerm t) = case t of
      TyApp t1 t2 -> fmap UTerm $ TyApp <$> replace kvs t1 <*> replace kvs t2
      TyVar v -> pure $ fromMaybe (UTerm t) $ List.lookup v kvs
      TyCon _ -> pure $ UTerm t
      TyPrim _ -> pure $ UTerm t
      TyArr t1 t2 -> fmap UTerm $ TyArr <$> replace kvs t1 <*> replace kvs t2
      TyTuple ts -> fmap UTerm $ TyTuple <$> traverse (replace kvs) ts
      TyLazy t -> fmap UTerm $ TyLazy <$> replace kvs t
      TyPtr t -> fmap UTerm $ TyPtr <$> replace kvs t
      TYPE rep -> fmap UTerm $ TYPE <$> replace kvs rep
      TyRep -> pure $ UTerm TyRep
      Rep rep -> pure $ UTerm $ Rep rep

class HasType t a where
  typeOf :: a -> t

instance HasType UType UType where
  typeOf (UVar v) = v ^. typeVar . idMeta
  typeOf (UTerm t) = case t of
    TyApp t1 _ -> case typeOf t1 of
      UTerm (TyArr _ k) -> k
      _ -> error "invalid kind"
    TyVar v -> v ^. idMeta
    TyCon c -> c ^. idMeta
    TyPrim p -> S.fromType (S.typeOf p)
    TyArr _ t2 -> typeOf t2
    TyTuple _ -> UTerm $ TYPE (UTerm $ Rep BoxedRep)
    TyLazy _ -> UTerm $ TYPE (UTerm $ Rep BoxedRep)
    TyPtr _ -> UTerm $ TYPE (UTerm $ Rep BoxedRep)
    TYPE rep -> UTerm $ TYPE rep
    TyRep -> UTerm TyRep
    Rep _ -> UTerm TyRep

instance HasType Type Type where
  typeOf = id

instance HasType t Void where
  typeOf = absurd

class WithUType a where
  withUType :: Lens' a UType

instance WithUType (With UType a) where
  withUType f (With t a) = (`With` a) <$> f t

instance WithUType Void where
  withUType _ a = absurd a
