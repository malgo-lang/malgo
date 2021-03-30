{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wno-orphans #-}
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
import Language.Malgo.TypeRep.Static (IsScheme, IsType (safeToType), Rep (..), TypeF (..))
import qualified Language.Malgo.TypeRep.Static as S
import Language.Malgo.UTerm
import Language.Malgo.Unify
import Text.Megaparsec (SourcePos)

----------
-- Type --
----------

deriveEq1 ''TypeF
deriveOrd1 ''TypeF
deriveShow1 ''TypeF

type UType = UTerm TypeF TypeVar

type Type = Fix TypeF

instance Pretty1 TypeF where
  liftPPrintPrec ppr l d (TyAppF t1 t2) =
    maybeParens (d > 10) $ sep [ppr l 10 t1, ppr l 11 t2]
  liftPPrintPrec _ _ _ (TyVarF v) = pprIdName v
  liftPPrintPrec ppr l d (TyConF c) = liftPPrintPrec ppr l d c
  liftPPrintPrec _ _ _ (TyPrimF p) = pPrint p
  liftPPrintPrec ppr l d (TyArrF t1 t2) =
    maybeParens (d > 10) $ ppr l 11 t1 <+> "->" <+> ppr l 10 t2
  liftPPrintPrec ppr l _ (TyTupleF ts) = parens $ sep $ punctuate "," $ map (ppr l 0) ts
  liftPPrintPrec ppr l _ (TyLazyF t) = braces $ ppr l 0 t
  liftPPrintPrec ppr l d (TyPtrF t) = maybeParens (d > 10) $ sep ["Ptr#", ppr l 11 t]
  liftPPrintPrec ppr l _ (TYPEF rep) = "TYPE" <+> ppr l 0 rep
  liftPPrintPrec _ _ _ TyRepF = "#Rep"
  liftPPrintPrec _ l _ (RepF rep) = pPrintPrec l 0 rep

instance Pretty a => Pretty (TypeF a) where
  pPrintPrec l d t = liftPPrintPrec pPrintPrec l d t

instance (IsType a) => IsType (TypeF a) where
  safeToType (TyAppF t1 t2) = S.TyApp <$> S.safeToType t1 <*> S.safeToType t2
  safeToType (TyVarF v) = S.TyVar <$> traverseOf idMeta S.safeToType v
  safeToType (TyConF c) = S.TyCon <$> traverseOf idMeta S.safeToType c
  safeToType (TyPrimF p) = Just $ S.TyPrim p
  safeToType (TyArrF t1 t2) = S.TyArr <$> S.safeToType t1 <*> S.safeToType t2
  safeToType (TyTupleF ts) = S.TyTuple <$> traverse S.safeToType ts
  safeToType (TyLazyF t) = S.TyLazy <$> S.safeToType t
  safeToType (TyPtrF t) = S.TyPtr <$> S.safeToType t
  safeToType (TYPEF rep) = S.TYPE <$> S.safeToType rep
  safeToType TyRepF = Just S.TyRep
  safeToType (RepF rep) = Just $ S.Rep rep
  fromType (S.TyApp t1 t2) = TyAppF (S.fromType t1) (S.fromType t2)
  fromType (S.TyVar v) = TyVarF (over idMeta (\k -> k ^. re S._Type) v)
  fromType (S.TyCon c) = TyConF (over idMeta (\k -> k ^. re S._Type) c)
  fromType (S.TyPrim p) = TyPrimF p
  fromType (S.TyArr t1 t2) = TyArrF (S.fromType t1) (S.fromType t2)
  fromType (S.TyTuple ts) = TyTupleF (map S.fromType ts)
  fromType (S.TyLazy t) = TyLazyF (S.fromType t)
  fromType (S.TyPtr t) = TyPtrF (S.fromType t)
  fromType (S.TYPE rep) = TYPEF $ S.fromType rep
  fromType S.TyRep = TyRepF
  fromType (S.Rep rep) = RepF rep

instance IsType a => S.HasType (TypeF a) where
  typeOf = S.typeOf . S.toType

newtype TypeVar = TypeVar {_typeVar :: Id UType}
  deriving newtype (Eq, Ord, Show, Generic, Hashable)

makeLenses ''TypeVar

instance HasUTerm TypeF TypeVar TypeVar where
  walkOn f (TypeVar x) = TypeVar <$> traverseOf idMeta f x

instance Pretty TypeVar where
  pPrint (TypeVar v) = "'" <> pPrint v

instance Unifiable1 TypeF where
  liftUnify _ x (TyAppF t11 t12) (TyAppF t21 t22) = pure (mempty, [With x $ t11 :~ t21, With x $ t12 :~ t22])
  liftUnify _ _ (TyVarF v1) (TyVarF v2) | v1 == v2 = pure (mempty, [])
  liftUnify _ _ (TyConF c1) (TyConF c2) | c1 == c2 = pure (mempty, [])
  liftUnify _ _ (TyPrimF p1) (TyPrimF p2) | p1 == p2 = pure (mempty, [])
  liftUnify _ x (TyArrF l1 r1) (TyArrF l2 r2) = pure (mempty, [With x $ l1 :~ l2, With x $ r1 :~ r2])
  liftUnify _ x (TyTupleF ts1) (TyTupleF ts2) = pure (mempty, zipWith (\t1 t2 -> With x $ t1 :~ t2) ts1 ts2)
  liftUnify _ x (TyLazyF t1) (TyLazyF t2) = pure (mempty, [With x $ t1 :~ t2])
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
  liftEquiv equiv (TyTupleF ts1) (TyTupleF ts2) = mconcat <$> zipWithM equiv ts1 ts2
  liftEquiv equiv (TyLazyF t1) (TyLazyF t2) = equiv t1 t2
  liftEquiv equiv (TyPtrF t1) (TyPtrF t2) = equiv t1 t2
  liftEquiv equiv (TYPEF rep1) (TYPEF rep2) = equiv rep1 rep2
  liftEquiv _ TyRepF TyRepF = Just mempty
  liftEquiv _ (RepF rep1) (RepF rep2) | rep1 == rep2 = Just mempty
  liftEquiv _ _ _ = Nothing
  liftFreevars freevars = foldMap freevars
  liftOccursCheck occursCheck v t = or $ fmap (occursCheck v) t

type TypeMap = HashMap TypeVar UType

newtype TypeUnifyT m a = TypeUnifyT {unTypeUnifyT :: StateT TypeMap m a}
  deriving newtype (Functor, Applicative, Monad, MonadState TypeMap, MonadUniq, MonadMalgo, MonadIO)

instance MonadTrans TypeUnifyT where
  lift m = TypeUnifyT $ lift m

runTypeUnifyT :: Monad m => TypeUnifyT m a -> m a
runTypeUnifyT (TypeUnifyT m) = evalStateT m mempty

instance (Monad m, MonadUniq m, MonadMalgo m) => MonadBind (UTerm TypeF TypeVar) (TypeUnifyT m) where
  lookupVar v = view (at v) <$> get
  freshVar = do
    rep <- TypeVar <$> newLocalId "r" (UTerm TyRepF)
    kind <- TypeVar <$> newLocalId "k" (UTerm $ TYPEF $ UVar rep)
    TypeVar <$> newLocalId "t" (UVar kind)
  bindVar x v t = do
    when (occursCheck v t) $ errorOn x $ "Occurs check:" <+> quotes (pPrint v) <+> "for" <+> pPrint t
    tKind <- typeOf t
    let cs = [With x $ v ^. typeVar . idMeta :~ tKind]
    solve cs
    at v ?= t
  zonk (UVar v) = do
    mterm <- lookupVar v
    mterm <- traverse zonk mterm
    pure $ fromMaybe (UVar v) mterm
  zonk (UTerm t) = UTerm <$> traverse zonk t

data Scheme = Forall [Id Type] (UTerm TypeF TypeVar)
  deriving stock (Eq, Ord, Show, Generic)

instance Pretty Scheme where
  pPrintPrec l _ (Forall vs t) = "forall" <+> sep (map pprIdName vs) <> "." <+> pPrintPrec l 0 t

instance HasUTerm TypeF TypeVar Scheme where
  walkOn f (Forall vs t) = Forall vs <$> walkOn f t

instance IsScheme Scheme where
  safeToScheme (Forall vs t) = do
    let vs' = map (over idMeta S.toType) vs
    t' <- safeToType =<< freeze t
    Just $ S.Forall vs' t'
  fromScheme (S.Forall vs t) = Forall (map (over idMeta S.fromType) vs) (unfreeze $ S.fromType t)

generalize :: (MonadUniq m, MonadBind UType m) => SourcePos -> HashSet TypeVar -> UType -> m Scheme
generalize x bound term = do
  {-
  let fvs = Set.toList $ unboundFreevars bound term
  as <- zipWithM toBound fvs [[c] | c <- ['a' ..]]
  zipWithM_ (\fv a -> bindVar x fv $ UTerm $ TyVar a) fvs as
  Forall as <$> zonkUTerm term
  -}
  zonkedTerm <- zonk term
  let fvs = HashSet.toList $ unboundFreevars bound zonkedTerm
  as <- zipWithM (toBound x) fvs [[c] | c <- ['a' ..]]
  zipWithM_ (\fv a -> bindVar x fv $ UTerm $ TyVarF a) fvs $ map (over idMeta unfreeze) as
  Forall as <$> zonk zonkedTerm

toBound :: (MonadUniq m, MonadBind UType m) => SourcePos -> TypeVar -> [Char] -> m (Id Type)
toBound x tv hint = do
  tvType <- defaultToBoxed x $ tv ^. typeVar . idMeta
  tvKind <- typeOf tvType
  case freeze tvKind of
    Just kind -> newLocalId hint kind
    Nothing -> errorDoc $ pPrint tvType

defaultToBoxed :: (Applicative m, MonadBind UType m) => SourcePos -> UType -> m UType
defaultToBoxed x (UVar v) = do
  vKind <- typeOf $ v ^. typeVar . idMeta
  case vKind of
    UTerm TyRepF -> bindVar x v (UTerm $ RepF BoxedRep) >> pure (UTerm $ RepF BoxedRep)
    _ -> do
      void $ defaultToBoxed x =<< typeOf (v ^. typeVar . idMeta)
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
    defaultToBoxed' (TyTupleF ts) = do
      ts <- traverse (defaultToBoxed x) ts
      pure $ TyTupleF ts
    defaultToBoxed' (TyLazyF t) = do
      t <- defaultToBoxed x t
      pure $ TyLazyF t
    defaultToBoxed' (TyPtrF t) = do
      t <- defaultToBoxed x t
      pure $ TyPtrF t
    defaultToBoxed' (TYPEF rep) = do
      rep <- defaultToBoxed x rep
      pure $ TYPEF rep
    defaultToBoxed' TyRepF = pure TyRepF
    defaultToBoxed' (RepF rep) = pure $ RepF rep

unboundFreevars :: Unifiable t => HashSet (Var t) -> t -> HashSet (Var t)
unboundFreevars bound t = HashSet.difference (freevars t) bound

generalizeMutRecs :: (MonadUniq m, MonadBind UType m) => SourcePos -> HashSet TypeVar -> [UType] -> m ([Id Type], [UType])
generalizeMutRecs x bound terms = do
  {-
  let fvs = Set.toList $ mconcat $ map (unboundFreevars bound) terms
  as <- zipWithM toBound fvs [[c] | c <- ['a' ..]]
  zipWithM_ (\fv a -> bindVar x fv $ UTerm $ TyVar a) fvs as
  (as,) <$> traverse zonkUTerm terms
  -}
  zonkedTerms <- traverse zonk terms
  let fvs = HashSet.toList $ mconcat $ map (unboundFreevars bound) zonkedTerms
  as <- zipWithM (toBound x) fvs [[c] | c <- ['a' ..]]
  zipWithM_ (\fv a -> bindVar x fv $ UTerm $ TyVarF a) fvs $ map (over idMeta unfreeze) as
  (as,) <$> traverse zonk zonkedTerms

instantiate :: (MonadBind UType m) => SourcePos -> Scheme -> m UType
instantiate x (Forall as t) = do
  avs <- traverse ?? as $ \a -> do
    let a' = over idMeta unfreeze a
    v <- UVar <$> freshVar @UType
    vKind <- typeOf v
    solve [With x $ a' ^. idMeta :~ vKind]
    pure (a', v)
  replace avs t
  where
    replace _ t@UVar {} = pure t
    replace kvs (UTerm t) = case t of
      TyAppF t1 t2 -> fmap UTerm $ TyAppF <$> replace kvs t1 <*> replace kvs t2
      TyVarF v -> pure $ fromMaybe (UTerm t) $ List.lookup v kvs
      TyConF _ -> pure $ UTerm t
      TyPrimF _ -> pure $ UTerm t
      TyArrF t1 t2 -> fmap UTerm $ TyArrF <$> replace kvs t1 <*> replace kvs t2
      TyTupleF ts -> fmap UTerm $ TyTupleF <$> traverse (replace kvs) ts
      TyLazyF t -> fmap UTerm $ TyLazyF <$> replace kvs t
      TyPtrF t -> fmap UTerm $ TyPtrF <$> replace kvs t
      TYPEF rep -> fmap UTerm $ TYPEF <$> replace kvs rep
      TyRepF -> pure $ UTerm TyRepF
      RepF rep -> pure $ UTerm $ RepF rep

class HasType a where
  typeOf :: Monad m => a -> m UType

instance HasType UType where
  typeOf (UVar v) = pure $ v ^. typeVar . idMeta
  typeOf (UTerm t) = case t of
    TyAppF t1 _ -> do
      typeOf t1 >>= \case
        UTerm (TyArrF _ k) -> pure k
        _ -> error "invalid kind"
    TyVarF v -> pure $ v ^. idMeta
    TyConF c -> pure $ c ^. idMeta
    TyPrimF p -> S.fromType <$> S.typeOf p
    TyArrF _ t2 -> typeOf t2
    TyTupleF _ -> pure $ UTerm $ TYPEF (UTerm $ RepF BoxedRep)
    TyLazyF _ -> pure $ UTerm $ TYPEF (UTerm $ RepF BoxedRep)
    TyPtrF _ -> pure $ UTerm $ TYPEF (UTerm $ RepF BoxedRep)
    TYPEF rep -> pure $ UTerm $ TYPEF rep
    TyRepF -> pure $ UTerm TyRepF
    RepF _ -> pure $ UTerm TyRepF

instance HasType Void where
  typeOf = absurd

class WithUType a where
  withUType :: Lens' a UType

instance WithUType (With UType a) where
  withUType f (With t a) = (`With` a) <$> f t

instance WithUType Void where
  withUType _ a = absurd a

pattern TyApp :: UTerm TypeF v -> UTerm TypeF v -> UTerm TypeF v
pattern TyApp t1 t2 = UTerm (TyAppF t1 t2)

pattern TyVar :: Id (UTerm TypeF v) -> UTerm TypeF v
pattern TyVar v = UTerm (TyVarF v)

pattern TyCon :: Id (UTerm TypeF v) -> UTerm TypeF v
pattern TyCon c = UTerm (TyConF c)

pattern TyPrim :: S.PrimT -> UTerm TypeF v
pattern TyPrim p = UTerm (TyPrimF p)

pattern TyArr :: UTerm TypeF v -> UTerm TypeF v -> UTerm TypeF v
pattern TyArr t1 t2 = UTerm (TyArrF t1 t2)

pattern TyTuple :: [UTerm TypeF v] -> UTerm TypeF v
pattern TyTuple ts = UTerm (TyTupleF ts)

pattern TyLazy :: UTerm TypeF v -> UTerm TypeF v
pattern TyLazy t = UTerm (TyLazyF t)

pattern TyPtr :: UTerm TypeF v -> UTerm TypeF v
pattern TyPtr t = UTerm (TyPtrF t)

pattern TYPE :: UTerm TypeF v -> UTerm TypeF v
pattern TYPE rep = UTerm (TYPEF rep)

pattern TyRep :: UTerm TypeF v
pattern TyRep = UTerm TyRepF

pattern Rep :: Rep -> UTerm TypeF v
pattern Rep rep = UTerm (RepF rep)
