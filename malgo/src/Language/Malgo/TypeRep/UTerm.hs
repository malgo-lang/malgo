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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Malgo.TypeRep.UTerm where

import Data.Binary (Binary)
import Data.Deriving
import Data.Fix
import qualified Data.Map as Map
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Pretty
import Language.Malgo.Prelude
import Language.Malgo.TypeRep.Static (PrimT (..))
import Language.Malgo.Unify

----------------------
-- Kind and HasKind --
----------------------

-- | Runtime representation
data Rep
  = -- | Boxed value
    BoxedRep
  | -- | Int32#
    Int32Rep
  | -- | Int64#
    Int64Rep
  | -- | Float#
    FloatRep
  | -- | Double#
    DoubleRep
  | -- | Char#
    CharRep
  | -- | String#
    StringRep
  deriving stock (Eq, Ord, Show, Generic)

instance Binary Rep

instance Pretty Rep where pPrint rep = text $ show rep

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

newtype KindVar = KindVar (Id ())
  deriving newtype (Eq, Ord, Show, Pretty)

type UKind = UTerm KindF KindVar

type Kind = Fix KindF

instance Var KindF KindVar where
  isRigid _ = False
  rigidName = lens (const "") const
  toRigidName = const ""
  toBound _ _ = pure $ Fix $ Type BoxedRep

instance Unifiable KindF KindVar where
  unify x (Type rep1) (Type rep2)
    | rep1 == rep2 = []
    | otherwise = errorWithMeta x $ unifyErrorMessage rep1 rep2
  unify x (KArr l1 r1) (KArr l2 r2) = [WithMeta x (l1 :~ l2), WithMeta x (r1 :~ r2)]
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

data TypeVar k = TypeVar
  { typeVarId :: Id k,
    typeVarRigidName :: String
  }
  deriving stock (Eq, Ord, Show)

instance Pretty k => Pretty (TypeVar k) where
  pPrint TypeVar {typeVarId = v, typeVarRigidName = ""} = pPrint v
  pPrint TypeVar {typeVarRigidName = name} = "'" <> text name

instance Pretty k => Var (TypeF k) (TypeVar k) where
  isRigid TypeVar {typeVarRigidName = ""} = False
  isRigid _ = True
  rigidName = lens typeVarRigidName (\v n -> v {typeVarRigidName = n})
  toRigidName (Fix v) = show $ pPrint v
  toBound TypeVar {typeVarId, typeVarRigidName} hint
    | null typeVarRigidName = do
      TypeVar v _ <- freshVar @(TypeF k)
      pure $ Fix $ TyVar (v & idName .~ hint & idMeta .~ typeVarId ^. idMeta)
    | otherwise = do
      TypeVar v _ <- freshVar @(TypeF k)
      pure $ Fix $ TyVar (v & idName .~ typeVarRigidName & idMeta .~ typeVarId ^. idMeta)

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
  unify x (TyApp t11 t12) (TyApp t21 t22) = [WithMeta x (t11 :~ t21), WithMeta x (t12 :~ t22)]
  unify x (TyVar v1) (TyVar v2)
    | v1 == v2 = []
    | otherwise = errorWithMeta x $ unifyErrorMessage v1 v2
  unify x (TyCon c1) (TyCon c2)
    | c1 == c2 = []
    | otherwise = errorWithMeta x $ unifyErrorMessage c1 c2
  unify x (TyPrim p1) (TyPrim p2)
    | p1 == p2 = []
    | otherwise = errorWithMeta x $ unifyErrorMessage p1 p2
  unify x (TyArr l1 r1) (TyArr l2 r2) = [WithMeta x (l1 :~ l2), WithMeta x (r1 :~ r2)]
  unify x (TyTuple ts1) (TyTuple ts2) = map (WithMeta x) $ zipWith (:~) ts1 ts2
  unify x (TyLazy t1) (TyLazy t2) = [WithMeta x (t1 :~ t2)]
  unify x (TyPtr t1) (TyPtr t2) = [WithMeta x (t1 :~ t2)]
  unify x t1 t2 = errorWithMeta x $ unifyErrorMessage t1 t2

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
    liftKindUnifyT $ solve [WithMeta x $ kindOf v :~ kindOf t]
    modify (Map.insert v t)

type TypeScheme k = UScheme (TypeF k) (TypeVar k)

class HasType t a | a -> t where
  typeOf :: a -> t

instance HasType (UTerm (TypeF k) (TypeVar k)) (UTerm (TypeF k) (TypeVar k)) where
  typeOf = id

instance HasType (Fix (TypeF k)) (Fix (TypeF k)) where
  typeOf = id

data WithUType a = WithUType a UType
  deriving stock (Eq, Show, Ord, Functor, Foldable)

instance Pretty a => Pretty (WithUType a) where
  pPrint (WithUType a t) = pPrint a <> ":" <> pPrint t

instance HasType UType (WithUType a) where
  typeOf (WithUType _ t) = t

instance HasUTerm (TypeF UKind) (TypeVar UKind) (WithUType a) where
  walkOn f (WithUType x t) = WithUType x <$> f t
