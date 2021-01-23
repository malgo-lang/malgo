{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Type where

import qualified Data.Set as Set
import Data.Store
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Pretty
import Language.Griff.Prelude

----------------------
-- Kind and HasKind --
----------------------

pattern Star :: Kind
pattern Star = Type Boxed

-- | Definition of `kind`
data Kind
  = -- | a kind
    Type Rep
  | -- | kind arrow (* -> *, * is a kind)
    KArr Kind Kind
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Store)

class HasKind a where
  kind :: (HasCallStack, MonadIO m) => a -> m (Maybe Kind)

instance HasKind a => HasKind (Id a) where
  kind = kind . view idMeta

instance HasKind Kind where
  kind = pure . Just

instance Pretty Kind where
  pPrintPrec _ _ (Type rep) = pPrint rep
  pPrintPrec l d (KArr k1 k2) =
    maybeParens (d > 10) $ pPrintPrec l 11 k1 <+> "->" <+> pPrintPrec l 10 k2

-- | Runtime representation
data Rep
  = -- | Boxed value
    Boxed
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
  deriving anyclass (Store)

instance Pretty Rep where pPrint rep = text $ show rep

---------------------
-- Primitive Types --
---------------------

data PrimT = Int32T | Int64T | FloatT | DoubleT | CharT | StringT
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Store)

instance Pretty PrimT where
  pPrint Int32T = "Int32#"
  pPrint Int64T = "Int64#"
  pPrint FloatT = "Float#"
  pPrint DoubleT = "Double#"
  pPrint CharT = "Char#"
  pPrint StringT = "String#"

instance HasKind PrimT where
  kind Int32T = pure $ Just $ Type Int32Rep
  kind Int64T = pure $ Just $ Type Int64Rep
  kind FloatT = pure $ Just $ Type FloatRep
  kind DoubleT = pure $ Just $ Type DoubleRep
  kind CharT = pure $ Just $ Type CharRep
  kind StringT = pure $ Just $ Type StringRep

----------
-- Type --
----------

data Scheme = Forall [TyVar] Type
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Store)

instance HasKind Scheme where
  kind (Forall _ t) = kind t

instance Pretty Scheme where
  pPrint (Forall vs t) = "forall" <+> sep (map pPrint vs) <> "." <+> pPrint t

type TyVar = Id Kind

type TyCon = Id Kind

data Type
  = TyApp Type Type
  | TyVar TyVar
  | TyCon TyCon
  | TyPrim PrimT
  | TyArr Type Type
  | TyTuple [Type]
  | TyLazy Type
  | TyPtr Type
  | TyMeta MetaTv
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Store)

_TyApp :: Prism' Type (Type, Type)
_TyApp = prism' (uncurry TyApp) $ \case
  TyApp t1 t2 -> Just (t1, t2)
  _ -> Nothing

_TyCon :: Prism' Type TyCon
_TyCon = prism' TyCon $ \case
  TyCon c -> Just c
  _ -> Nothing

_TyArr :: Prism' Type (Type, Type)
_TyArr = prism' (uncurry TyArr) $ \case
  TyArr t1 t2 -> Just (t1, t2)
  _ -> Nothing

_TyLazy :: Prism' Type Type
_TyLazy = prism' TyLazy $ \case
  TyLazy t -> Just t
  _ -> Nothing

instance HasKind Type where
  kind (TyApp t _) = do
    mk <- kind t
    case mk of
      Just (KArr _ k) -> pure $ Just k
      _ -> error "invalid kind" -- TODO: 位置情報を元にした親切なエラーメッセージ
  kind (TyVar t) = kind t
  kind (TyCon c) = kind c
  kind (TyPrim p) = kind p
  kind (TyArr _ _) = pure $ Just $ Type Boxed
  kind (TyTuple _) = pure $ Just $ Type Boxed
  kind (TyLazy _) = pure $ Just $ Type Boxed
  kind (TyPtr _) = pure $ Just $ Type Boxed
  kind (TyMeta tv) = kind tv

instance Pretty Type where
  pPrintPrec l d (TyApp t1 t2) =
    maybeParens (d > 10) $ sep [pPrintPrec l 10 t1, pPrintPrec l 11 t2]
  pPrintPrec _ _ (TyVar v) = pPrint v
  pPrintPrec _ _ (TyCon c) = pPrint c
  pPrintPrec _ _ (TyPrim p) = pPrint p
  pPrintPrec l d (TyArr t1 t2) =
    maybeParens (d > 10) $ pPrintPrec l 11 t1 <+> "->" <+> pPrintPrec l 10 t2
  pPrintPrec _ _ (TyTuple ts) = parens $ sep $ punctuate "," $ map pPrint ts
  pPrintPrec _ _ (TyLazy t) = braces $ pPrint t
  pPrintPrec l d (TyPtr t) = maybeParens (d > 10) $ sep ["Ptr#", pPrintPrec l 11 t]
  pPrintPrec _ _ (TyMeta tv) = pPrint tv

-------------------
-- Type variable --
-------------------

data MetaTv = MetaTv
  { _metaTvUniq :: Int,
    _metaTvKind :: IORef (Maybe Kind),
    _metaTvRigidName :: String,
    _metaTvTypeRef :: IORef (Maybe Type)
  }

isRigid :: MetaTv -> Bool
isRigid MetaTv {_metaTvRigidName = ""} = False
isRigid _ = True

rigidName :: MetaTv -> String
rigidName MetaTv {_metaTvRigidName = n} = n

instance Eq MetaTv where
  (MetaTv u1 _ _ _) == (MetaTv u2 _ _ _) = u1 == u2

instance Ord MetaTv where
  (MetaTv u1 _ _ _) `compare` (MetaTv u2 _ _ _) = u1 `compare` u2

instance Show MetaTv where
  show (MetaTv u _ _ _) = "_" <> show u

instance Pretty MetaTv where
  pPrint (MetaTv u _ [] _) = "'" <> pPrint u
  pPrint (MetaTv _ _ rigidName _) = "'" <> text rigidName

instance HasKind MetaTv where
  kind MetaTv {_metaTvKind} = readIORef _metaTvKind

instance Store MetaTv where
  size = error "Store MetaTv"
  poke _ = error "Store MetaTv"
  peek = error "Store MetaTv"

---------------------------
-- Read and Write MetaTv --
---------------------------

newMetaTv :: (MonadUniq f, MonadIO f) => Maybe Kind -> String -> f MetaTv
newMetaTv k rigidName = MetaTv <$> getUniq <*> newIORef k <*> pure rigidName <*> newIORef Nothing

readMetaTv :: MonadIO m => MetaTv -> m (Maybe Type)
readMetaTv (MetaTv _ _ _ ref) = readIORef ref

writeMetaTv :: (HasCallStack, MonadIO m) => MetaTv -> Type -> m ()
writeMetaTv tv@(MetaTv _ kindRef _ typeRef) t = do
  mktv <- kind tv
  mkt <- kind t
  -- occurs checkが必要？
  case (mktv, mkt) of
    (Just ktv, Just kt)
      | ktv == kt -> writeIORef typeRef (Just t)
      | otherwise -> errorDoc $ "Panic!" <+> "Kind of" <+> pPrint t <+> "is not" <+> pPrint ktv
    (Just _, Nothing) -> case t of
      TyMeta tv' -> writeMetaTv tv' (TyMeta tv)
      _ -> bug Unreachable
    (Nothing, Just kt) -> writeIORef kindRef (Just kt) >> writeIORef typeRef (Just t)
    (Nothing, Nothing) -> writeIORef typeRef (Just t)

metaTvs :: Type -> Set MetaTv
metaTvs (TyApp t1 t2) = metaTvs t1 <> metaTvs t2
metaTvs (TyArr t1 t2) = metaTvs t1 <> metaTvs t2
metaTvs (TyTuple ts) = mconcat $ map metaTvs ts
metaTvs (TyLazy t) = metaTvs t
metaTvs (TyPtr t) = metaTvs t
metaTvs (TyMeta tv) = Set.singleton tv
metaTvs _ = mempty

metaTvsScheme :: Scheme -> Set MetaTv
metaTvsScheme (Forall _ t) = metaTvs t

-------------
-- Zonking --
-------------

zonkScheme :: MonadIO f => Scheme -> f Scheme
zonkScheme (Forall as t) = Forall as <$> zonkType t

zonkType :: MonadIO f => Type -> f Type
zonkType (TyMeta tv) = do
  mty <- readMetaTv tv
  case mty of
    Just ty -> zonkType ty
    Nothing -> pure $ TyMeta tv
zonkType (TyApp t1 t2) = TyApp <$> zonkType t1 <*> zonkType t2
zonkType (TyArr t1 t2) = TyArr <$> zonkType t1 <*> zonkType t2
zonkType (TyTuple ts) = TyTuple <$> traverse zonkType ts
zonkType (TyLazy t) = TyLazy <$> zonkType t
zonkType (TyPtr t) = TyPtr <$> zonkType t
zonkType t = pure t

-------------------
-- HasType class --
-------------------

class HasType a where
  toType :: Getter a Type
  overType :: (MonadIO m, MonadUniq m) => (Type -> m Type) -> a -> m a

instance HasType Type where
  toType = to id
  overType = id

instance HasType Scheme where
  toType = to $ \(Forall _ t) -> t
  overType f (Forall vs t) = Forall vs <$> f t

instance HasType a => HasType (Id a) where
  toType = idMeta . toType
  overType f n = traverseOf idMeta (overType f) n

data WithType a = WithType a Type
  deriving stock (Eq, Show, Ord, Functor, Foldable)

instance Pretty a => Pretty (WithType a) where
  pPrint (WithType a t) = pPrint a <> ":" <> pPrint t

instance HasType (WithType a) where
  toType = to $ \(WithType _ t) -> t
  overType f (WithType x t) = WithType x <$> f t

----------------
-- split Type --
----------------

splitCon :: Type -> (TyCon, [Type])
splitCon (TyCon con) = (con, [])
splitCon (TyApp t1 t2) = let (dataCon, ts) = splitCon t1 in (dataCon, ts <> [t2])
splitCon _ = bug Unreachable

splitTyArr :: Type -> ([Type], Type)
splitTyArr (TyArr t1 t2) = let (ps, r) = splitTyArr t2 in (t1 : ps, r)
splitTyArr (TyMeta _) = bug Unreachable
splitTyArr t = ([], t)
