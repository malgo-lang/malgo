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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Functor.Foldable
import qualified Data.HashSet as HashSet
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Void
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Pretty
import Language.Malgo.Prelude
import Language.Malgo.TypeRep.Static (IsType (fromType, safeToType), Rep (..), Scheme (Forall), TypeF (..))
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

instance Pretty t => Pretty (TypeF t) where
  pPrintPrec l d (TyAppF t1 t2) =
    maybeParens (d > 10) $ sep [pPrintPrec l 10 t1, pPrintPrec l 11 t2]
  pPrintPrec _ _ (TyVarF v) = pprIdName v
  pPrintPrec l d (TyConF c) = pPrintPrec l d c
  pPrintPrec _ _ (TyPrimF p) = pPrint p
  pPrintPrec l d (TyArrF t1 t2) =
    maybeParens (d > 10) $ pPrintPrec l 11 t1 <+> "->" <+> pPrintPrec l 10 t2
  pPrintPrec l _ (TyTupleF ts) = parens $ sep $ punctuate "," $ map (pPrintPrec l 0) ts
  pPrintPrec l _ (TyRecordF kvs) = braces $ sep $ punctuate "," $ map (\(k, v) -> pPrintPrec l 0 k <> ":" <+> pPrintPrec l 0 v) $ Map.toList kvs
  pPrintPrec l _ (TyLazyF t) = braces $ pPrintPrec l 0 t
  pPrintPrec l d (TyPtrF t) = maybeParens (d > 10) $ sep ["Ptr#", pPrintPrec l 11 t]
  pPrintPrec l _ (TYPEF rep) = "TYPE" <+> pPrintPrec l 0 rep
  pPrintPrec _ _ TyRepF = "#Rep"
  pPrintPrec l _ (RepF rep) = pPrintPrec l 0 rep

instance (IsType a) => IsType (TypeF a) where
  safeToType = fmap embed . traverse safeToType
  fromType = fmap fromType . project

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
  liftUnify _ x (TyRecordF kts1) (TyRecordF kts2)
    | Map.keys kts1 == Map.keys kts2 = pure (mempty, zipWith (\t1 t2 -> With x $ t1 :~ t2) (Map.elems kts1) (Map.elems kts2))
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
  deriving newtype (Functor, Applicative, Monad, MonadState TypeMap, MonadUniq, MonadMalgo, MonadIO, MonadFail)

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
    tKind <- kindOf t
    let cs = [With x $ v ^. typeVar . idMeta :~ tKind]
    solve cs
    at v ?= t
  zonk (UVar v) = do
    mterm <- lookupVar v
    mterm <- traverse zonk mterm
    pure $ fromMaybe (UVar v) mterm
  zonk (UTerm t) = UTerm <$> traverse zonk t

generalize :: (MonadUniq m, MonadBind UType m) => SourcePos -> HashSet TypeVar -> UType -> m (Scheme UType)
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
  zipWithM_ (\fv a -> bindVar x fv $ UTerm $ TyVarF a) fvs as
  Forall as <$> zonk zonkedTerm

toBound :: (MonadUniq m, MonadBind UType m) => SourcePos -> TypeVar -> [Char] -> m (Id UType)
toBound x tv hint = do
  tvType <- defaultToBoxed x $ tv ^. typeVar . idMeta
  tvKind <- kindOf tvType
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
    defaultToBoxed' (TyTupleF ts) = do
      ts <- traverse (defaultToBoxed x) ts
      pure $ TyTupleF ts
    defaultToBoxed' (TyRecordF kvs) = do
      kvs <- traverse (defaultToBoxed x) kvs
      pure $ TyRecordF kvs
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

generalizeMutRecs :: (MonadUniq m, MonadBind UType m) => SourcePos -> HashSet TypeVar -> [UType] -> m ([Id UType], [UType])
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
  zipWithM_ (\fv a -> bindVar x fv $ UTerm $ TyVarF a) fvs as
  (as,) <$> traverse zonk zonkedTerms

instantiate :: (MonadBind UType m) => SourcePos -> Scheme UType -> m UType
instantiate x (Forall as t) = do
  avs <- traverse ?? as $ \a -> do
    v <- UVar <$> freshVar @UType
    vKind <- kindOf v
    solve [With x $ a ^. idMeta :~ vKind]
    pure (a, v)
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
      TyRecordF kts -> fmap UTerm $ TyRecordF <$> traverse (replace kvs) kts
      TyLazyF t -> fmap UTerm $ TyLazyF <$> replace kvs t
      TyPtrF t -> fmap UTerm $ TyPtrF <$> replace kvs t
      TYPEF rep -> fmap UTerm $ TYPEF <$> replace kvs rep
      TyRepF -> pure $ UTerm TyRepF
      RepF rep -> pure $ UTerm $ RepF rep

class HasType a where
  typeOf :: Monad m => a -> m UType

class HasKind a where
  kindOf :: Monad m => a -> m UType

instance HasKind UType where
  kindOf (UVar v) = pure $ v ^. typeVar . idMeta
  kindOf (UTerm t) = case t of
    TyAppF t1 _ -> do
      kindOf t1 >>= \case
        UTerm (TyArrF _ k) -> pure k
        _ -> error "invalid kind"
    TyVarF v -> pure $ v ^. idMeta
    TyConF c -> pure $ c ^. idMeta
    TyPrimF p -> S.fromType <$> S.kindOf p
    TyArrF _ t2 -> kindOf t2
    TyTupleF _ -> pure $ UTerm $ TYPEF (UTerm $ RepF BoxedRep)
    TyRecordF _ -> pure $ UTerm $ TYPEF (UTerm $ RepF BoxedRep)
    TyLazyF _ -> pure $ UTerm $ TYPEF (UTerm $ RepF BoxedRep)
    TyPtrF _ -> pure $ UTerm $ TYPEF (UTerm $ RepF BoxedRep)
    TYPEF rep -> pure $ UTerm $ TYPEF rep
    TyRepF -> pure $ UTerm TyRepF
    RepF _ -> pure $ UTerm TyRepF

instance HasType Void where
  typeOf = absurd

instance HasKind Void where
  kindOf = absurd

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

pattern TyRecord :: Map.Map (Id ()) (UTerm TypeF v) -> UTerm TypeF v
pattern TyRecord kts = UTerm (TyRecordF kts)

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
