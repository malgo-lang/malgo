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
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Malgo.TypeRep.UTerm where

import Data.Binary (Binary)
import Data.Deriving
import Data.Fix
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Void
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Pretty
import Language.Malgo.Prelude
import Language.Malgo.TypeRep.Static (IsKind (fromKind, safeToKind), IsScheme, IsType (safeToType), PrimT (..), Rep (..))
import qualified Language.Malgo.TypeRep.Static as S
import Language.Malgo.Unify

----------
-- Kind --
----------

-- | Definition of `kind`
data KindF a
  = -- | a kind
    Type Rep
  | -- | kind arrow (* -> *, * is a kind)
    KArr a a
  deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

instance Binary a => Binary (KindF a)

deriveEq1 ''KindF
deriveOrd1 ''KindF
deriveShow1 ''KindF

instance Pretty1 KindF where
  liftPPrintPrec _ _ _ (Type rep) = pPrint rep
  liftPPrintPrec ppr l d (KArr k1 k2) =
    maybeParens (d > 10) $ ppr l 11 k1 <+> "->" <+> ppr l 10 k2

instance Pretty a => Pretty (KindF a) where
  pPrintPrec l d k = liftPPrintPrec pPrintPrec l d k

instance IsKind a => IsKind (KindF a) where
  safeToKind (Type rep) = Just $ S.TYPE rep
  safeToKind (KArr k1 k2) = S.KArr <$> safeToKind k1 <*> safeToKind k2
  fromKind (S.TYPE rep) = Type rep
  fromKind (S.KArr k1 k2) = KArr (fromKind k1) (fromKind k2)

newtype KindVar = KindVar (Id ())
  deriving newtype (Eq, Ord, Show, Pretty)

type UKind = UTerm KindF KindVar

type Kind = Fix KindF

instance Var KindVar where
  isRigid _ = False
  rigidName = lens (const "") const

instance Unifiable KindF KindVar where
  unify x (Type rep1) (Type rep2)
    | rep1 == rep2 = []
    | otherwise = errorWithMeta x $ unifyErrorMessage rep1 rep2
  unify x (KArr l1 r1) (KArr l2 r2) = [With x (l1 :~ l2), With x (r1 :~ r2)]
  unify x k1 k2 = errorWithMeta x $ unifyErrorMessage k1 k2

type KindMap = Map KindVar (UTerm KindF KindVar)

newtype KindUnifyT m a = KindUnifyT {unKindUnifyT :: StateT KindMap m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadState KindMap, MonadUniq, MonadMalgo, MonadIO)

runKindUnifyT :: Monad m => KindUnifyT m a -> m a
runKindUnifyT (KindUnifyT m) = evalStateT m mempty

instance (Monad m, MonadUniq m) => MonadBind KindF KindVar (KindUnifyT m) where
  lookupVar v = Map.lookup v <$> get
  freshVar = KindVar <$> newLocalId "k" ()
  bindVar x v k = do
    occursCheck x v k
    modify (Map.insert v k)

class HasKind k t | t -> k where
  kindOf :: t -> k

instance HasKind UKind UKind where
  kindOf = id

instance HasKind Kind Kind where
  kindOf = id

bindUnknownToBoxed :: MonadBind KindF KindVar m => UTerm KindF KindVar -> m (UTerm KindF KindVar)
bindUnknownToBoxed term = do
  zonkedTerm <- zonkUTerm term
  let fvs = freevars zonkedTerm
  traverse_ (\fv -> bindVar () fv (UTerm $ Type BoxedRep)) fvs
  zonkUTerm zonkedTerm

----------
-- Type --
----------

-- | Definition of Type
data TypeF k a
  = TyApp a a
  | TyVar (Id k)
  | TyCon (Id k)
  | TyPrim PrimT
  | TyArr a a
  | TyTuple [a]
  | TyLazy a
  | TyPtr a
  deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

deriveEq1 ''TypeF
deriveOrd1 ''TypeF
deriveShow1 ''TypeF

type UType = UTerm (TypeF UKind) (TypeVar UKind)

type Type = Fix (TypeF Kind)

instance Pretty k => Pretty1 (TypeF k) where
  liftPPrintPrec ppr l d (TyApp t1 t2) =
    maybeParens (d > 10) $ sep [ppr l 10 t1, ppr l 11 t2]
  liftPPrintPrec _ _ _ (TyVar v) = pPrint v
  liftPPrintPrec _ _ _ (TyCon c) = pPrint c
  liftPPrintPrec _ _ _ (TyPrim p) = pPrint p
  liftPPrintPrec ppr l d (TyArr t1 t2) =
    maybeParens (d > 10) $ ppr l 11 t1 <+> "->" <+> ppr l 10 t2
  liftPPrintPrec ppr l _ (TyTuple ts) = parens $ sep $ punctuate "," $ map (ppr l 0) ts
  liftPPrintPrec ppr l _ (TyLazy t) = braces $ ppr l 0 t
  liftPPrintPrec ppr l d (TyPtr t) = maybeParens (d > 10) $ sep ["Ptr#", ppr l 11 t]

instance (Pretty k, Pretty v) => Pretty (TypeF k v) where
  pPrintPrec l d t = liftPPrintPrec pPrintPrec l d t

instance (IsKind k, IsType a) => IsType (TypeF k a) where
  safeToType (TyApp t1 t2) = S.TyApp <$> S.safeToType t1 <*> S.safeToType t2
  safeToType (TyVar v) = S.TyVar <$> traverseOf idMeta S.safeToKind v
  safeToType (TyCon c) = S.TyCon <$> traverseOf idMeta S.safeToKind c
  safeToType (TyPrim p) = Just $ S.TyPrim p
  safeToType (TyArr t1 t2) = S.TyArr <$> S.safeToType t1 <*> S.safeToType t2
  safeToType (TyTuple ts) = S.TyTuple <$> traverse S.safeToType ts
  safeToType (TyLazy t) = S.TyLazy <$> S.safeToType t
  safeToType (TyPtr t) = S.TyPtr <$> S.safeToType t
  fromType (S.TyApp t1 t2) = TyApp (S.fromType t1) (S.fromType t2)
  fromType (S.TyVar v) = TyVar (over idMeta (\k -> k ^. re S._Kind) v)
  fromType (S.TyCon c) = TyCon (over idMeta (\k -> k ^. re S._Kind) c)
  fromType (S.TyPrim p) = TyPrim p
  fromType (S.TyArr t1 t2) = TyArr (S.fromType t1) (S.fromType t2)
  fromType (S.TyTuple ts) = TyTuple (map S.fromType ts)
  fromType (S.TyLazy t) = TyLazy (S.fromType t)
  fromType (S.TyPtr t) = TyPtr (S.fromType t)

instance (IsKind k, IsType v) => S.HasType (TypeF k v) where
  typeOf = S.toType

data TypeVar k = TypeVar
  { typeVarId :: Id k,
    typeVarRigidName :: String
  }
  deriving stock (Eq, Ord, Show)

instance Pretty k => Pretty (TypeVar k) where
  pPrint TypeVar {typeVarId = v, typeVarRigidName = ""} = "'" <> pPrint v
  pPrint TypeVar {typeVarRigidName = name} = "'" <> text name

instance Pretty k => Var (TypeVar k) where
  isRigid TypeVar {typeVarRigidName = ""} = False
  isRigid _ = True
  rigidName = lens typeVarRigidName (\v n -> v {typeVarRigidName = n})

instance HasKind k (TypeVar k) where
  kindOf v = typeVarId v ^. idMeta

instance HasKind Kind PrimT where
  kindOf Int32T = Fix $ Type Int32Rep
  kindOf Int64T = Fix $ Type Int64Rep
  kindOf FloatT = Fix $ Type FloatRep
  kindOf DoubleT = Fix $ Type DoubleRep
  kindOf CharT = Fix $ Type CharRep
  kindOf StringT = Fix $ Type StringRep

instance HasKind UKind UType where
  kindOf (UVar v) = typeVarId v ^. idMeta
  kindOf (UTerm t) = case t of
    TyApp t1 _ -> case kindOf t1 of
      UTerm (KArr _ k) -> k
      _ -> error "invalid kind"
    TyVar v -> v ^. idMeta
    TyCon c -> c ^. idMeta
    TyPrim p -> unfreeze $ kindOf p
    TyArr _ _ -> UTerm $ Type BoxedRep
    TyTuple _ -> UTerm $ Type BoxedRep
    TyLazy _ -> UTerm $ Type BoxedRep
    TyPtr _ -> UTerm $ Type BoxedRep

instance HasKind Kind Type where
  kindOf (Fix (TyApp t1 _)) = case kindOf t1 of
    Fix (KArr _ k) -> k
    _ -> error "invalid kind"
  kindOf (Fix (TyVar v)) = v ^. idMeta
  kindOf (Fix (TyCon c)) = c ^. idMeta
  kindOf (Fix (TyPrim p)) = kindOf p
  kindOf (Fix (TyArr _ _)) = Fix $ Type BoxedRep
  kindOf (Fix (TyTuple _)) = Fix $ Type BoxedRep
  kindOf (Fix (TyLazy _)) = Fix $ Type BoxedRep
  kindOf (Fix (TyPtr _)) = Fix $ Type BoxedRep

instance HasUTerm KindF KindVar UType where
  walkOn f (UVar v) =
    f (typeVarId v ^. idMeta) <&> \k ->
      UVar v {typeVarId = typeVarId v & idMeta .~ k}
  walkOn f (UTerm t) =
    UTerm <$> case t of
      TyVar v ->
        f (v ^. idMeta) <&> \k ->
          TyVar (v & idMeta .~ k)
      TyCon c ->
        f (c ^. idMeta) <&> \k ->
          TyCon (c & idMeta .~ k)
      _ -> traverse (walkOn f) t

instance (Pretty k, Eq k) => Unifiable (TypeF k) (TypeVar k) where
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
  unify x t1 t2 = errorWithMeta x $ unifyErrorMessage t1 t2

freezeKind :: Fix (TypeF UKind) -> Maybe (Fix (TypeF Kind))
freezeKind (Fix t) =
  Fix
    <$> case t of
      TyApp t1 t2 -> TyApp <$> freezeKind t1 <*> freezeKind t2
      TyVar v -> TyVar <$> traverseOf idMeta freeze v
      TyCon c -> TyCon <$> traverseOf idMeta freeze c
      TyPrim p -> pure $ TyPrim p
      TyArr t1 t2 -> TyArr <$> freezeKind t1 <*> freezeKind t2
      TyTuple ts -> TyTuple <$> traverse freezeKind ts
      TyLazy t -> TyLazy <$> freezeKind t
      TyPtr t -> TyPtr <$> freezeKind t

unfreezeKind :: Fix (TypeF Kind) -> Fix (TypeF UKind)
unfreezeKind (Fix t) =
  Fix $
    case t of
      TyApp t1 t2 -> TyApp (unfreezeKind t1) (unfreezeKind t2)
      TyVar v -> TyVar $ over idMeta unfreeze v
      TyCon c -> TyCon $ over idMeta unfreeze c
      TyPrim p -> TyPrim p
      TyArr t1 t2 -> TyArr (unfreezeKind t1) (unfreezeKind t2)
      TyTuple ts -> TyTuple $ map unfreezeKind ts
      TyLazy t -> TyLazy $ unfreezeKind t
      TyPtr t -> TyPtr $ unfreezeKind t

freezeKind' :: UTerm (TypeF UKind) (TypeVar UKind) -> Maybe (UTerm (TypeF Kind) (TypeVar Kind))
freezeKind' (UVar v) = UVar <$> traverseOf (typeVarIdLens . idMeta) freeze v
  where
    typeVarIdLens = lens typeVarId (\v x -> v {typeVarId = x})
freezeKind' (UTerm t) =
  UTerm
    <$> case t of
      TyApp t1 t2 -> TyApp <$> freezeKind' t1 <*> freezeKind' t2
      TyVar v -> TyVar <$> traverseOf idMeta freeze v
      TyCon c -> TyCon <$> traverseOf idMeta freeze c
      TyPrim p -> pure $ TyPrim p
      TyArr t1 t2 -> TyArr <$> freezeKind' t1 <*> freezeKind' t2
      TyTuple ts -> TyTuple <$> traverse freezeKind' ts
      TyLazy t -> TyLazy <$> freezeKind' t
      TyPtr t -> TyPtr <$> freezeKind' t

unfreezeKind' :: UTerm (TypeF Kind) (TypeVar Kind) -> UTerm (TypeF UKind) (TypeVar UKind)
unfreezeKind' (UVar v) = UVar (over (typeVarIdLens . idMeta) unfreeze v)
  where
    typeVarIdLens = lens typeVarId (\v x -> v {typeVarId = x})
unfreezeKind' (UTerm t) =
  UTerm $
    case t of
      TyApp t1 t2 -> TyApp (unfreezeKind' t1) (unfreezeKind' t2)
      TyVar v -> TyVar $ over idMeta unfreeze v
      TyCon c -> TyCon $ over idMeta unfreeze c
      TyPrim p -> TyPrim p
      TyArr t1 t2 -> TyArr (unfreezeKind' t1) (unfreezeKind' t2)
      TyTuple ts -> TyTuple $ map unfreezeKind' ts
      TyLazy t -> TyLazy $ unfreezeKind' t
      TyPtr t -> TyPtr $ unfreezeKind' t

type TypeMap = Map (TypeVar UKind) (UTerm (TypeF UKind) (TypeVar UKind))

newtype TypeUnifyT m a = TypeUnifyT {unTypeUnifyT :: StateT TypeMap (KindUnifyT m) a}
  deriving newtype (Functor, Applicative, Monad, MonadState TypeMap, MonadUniq, MonadMalgo, MonadIO)

instance MonadTrans TypeUnifyT where
  lift m = TypeUnifyT $ lift $ lift m

liftKindUnifyT :: Monad m => KindUnifyT m a -> TypeUnifyT m a
liftKindUnifyT m = TypeUnifyT $ lift m

runTypeUnifyT :: Monad m => TypeUnifyT m a -> KindUnifyT m a
runTypeUnifyT (TypeUnifyT m) = evalStateT m mempty

instance (Monad m, MonadUniq m, MonadIO m) => MonadBind (TypeF UKind) (TypeVar UKind) (TypeUnifyT m) where
  lookupVar v = Map.lookup v <$> get
  freshVar = do
    kind <- liftKindUnifyT freshVar
    TypeVar <$> newLocalId "t" (UVar kind) <*> pure ""
  bindVar x v t = do
    occursCheck x v t
    liftKindUnifyT $ solve [With x $ kindOf v :~ kindOf t]
    modify (Map.insert v t)

data Scheme k = Forall [Id k] (UTerm (TypeF k) (TypeVar k))
  deriving stock (Eq, Ord, Show, Generic)

instance Pretty k => Pretty (Scheme k) where
  pPrintPrec l _ (Forall vs t) = "forall" <+> sep (map (pPrintPrec l 0) vs) <> "." <+> pPrintPrec l 0 t

instance HasUTerm (TypeF k) (TypeVar k) (Scheme k) where
  walkOn f (Forall vs t) = Forall vs <$> walkOn f t

instance HasUTerm KindF KindVar (Scheme UKind) where
  walkOn f (Forall vs t) = Forall <$> traverse (idMeta f) vs <*> walkOn f t

instance IsKind k => IsScheme (Scheme k) where
  safeToScheme (Forall vs t) = do
    let vs' = map (over idMeta S.toKind) vs
    t' <- safeToType =<< freeze t
    Just $ S.Forall vs' t'
  fromScheme (S.Forall vs t) = Forall (map (over idMeta S.fromKind) vs) (unfreeze $ S.fromType t)

generalize :: (MonadUniq m, MonadBind (TypeF k) (TypeVar k) m, Pretty x, Ord k) => x -> Set (TypeVar k) -> UTerm (TypeF k) (TypeVar k) -> m (Scheme k)
generalize x bound term = do
  {-
  let fvs = Set.toList $ unboundFreevars bound term
  as <- zipWithM toBound fvs [[c] | c <- ['a' ..]]
  zipWithM_ (\fv a -> bindVar x fv $ UTerm $ TyVar a) fvs as
  Forall as <$> zonkUTerm term
  -}
  zonkedTerm <- zonkUTerm term
  let fvs = Set.toList $ unboundFreevars bound zonkedTerm
  as <- zipWithM toBound fvs [[c] | c <- ['a' ..]]
  zipWithM_ (\fv a -> bindVar x fv $ UTerm $ TyVar a) fvs as
  Forall as <$> zonkUTerm zonkedTerm

toBound :: MonadUniq m => TypeVar k -> [Char] -> m (Id k)
toBound TypeVar {typeVarId, typeVarRigidName} hint
  | null typeVarRigidName = newLocalId hint (typeVarId ^. idMeta)
  | otherwise = newLocalId typeVarRigidName (typeVarId ^. idMeta)

unboundFreevars :: (Ord v, Foldable t) => Set v -> UTerm t v -> Set v
unboundFreevars bound t = freevars t Set.\\ bound

generalizeMutRecs :: (MonadUniq m, MonadBind (TypeF k) (TypeVar k) m, Pretty x, Ord k) => x -> Set (TypeVar k) -> [UTerm (TypeF k) (TypeVar k)] -> m ([Id k], [UTerm (TypeF k) (TypeVar k)])
generalizeMutRecs x bound terms = do
  {-
  let fvs = Set.toList $ mconcat $ map (unboundFreevars bound) terms
  as <- zipWithM toBound fvs [[c] | c <- ['a' ..]]
  zipWithM_ (\fv a -> bindVar x fv $ UTerm $ TyVar a) fvs as
  (as,) <$> traverse zonkUTerm terms
  -}
  zonkedTerms <- traverse zonkUTerm terms
  let fvs = Set.toList $ mconcat $ map (unboundFreevars bound) zonkedTerms
  as <- zipWithM toBound fvs [[c] | c <- ['a' ..]]
  zipWithM_ (\fv a -> bindVar x fv $ UTerm $ TyVar a) fvs as
  (as,) <$> traverse zonkUTerm zonkedTerms

instantiate :: (MonadBind (TypeF k) (TypeVar k) m, Pretty k, Eq k) => Bool -> Scheme k -> m (UTerm (TypeF k) (TypeVar k))
instantiate isRigid (Forall as t) = do
  vs <- traverse ?? as $ \a -> do
    v <- freshVar
    if isRigid
      then pure $ UVar $ v & rigidName .~ toRigidName a
      else pure $ UVar v
  replace (zip as vs) t
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
    toRigidName = view idName

class HasType t a where
  typeOf :: a -> t

instance HasType (UTerm (TypeF k) (TypeVar k)) (UTerm (TypeF k) (TypeVar k)) where
  typeOf = id

instance HasType (Fix (TypeF k)) (Fix (TypeF k)) where
  typeOf = id

instance HasType t Void where
  typeOf = absurd

type WithUType a = With UType a

instance HasType rep t => HasType rep (With t a) where
  typeOf (With t _) = typeOf t

-- instance HasUTerm (TypeF UKind) (TypeVar UKind) (WithUType a) where
--   walkOn f (With x t) = With <$> f x <*> pure t
