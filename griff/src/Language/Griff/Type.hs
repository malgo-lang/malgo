{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Type where

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
  kind :: HasCallStack => a -> Kind

instance HasKind a => HasKind (Id a) where
  kind = kind . view idMeta

instance HasKind Kind where
  kind = id

instance Pretty Kind where
  pPrintPrec _ _ (Type rep) = pPrint rep
  pPrintPrec l d (KArr k1 k2) =
    maybeParens (d > 10) $ pPrintPrec l 11 k1 <+> "->" <+> pPrintPrec l 10 k2

-- | Runtime representation
data Rep
  = -- | Boxed value
    Boxed
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Store)

instance Pretty Rep where pPrint rep = text $ show rep

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

-------------------
-- Type variable --
-------------------

data MetaTv = MetaTv
  { _metaTvUniq :: Int,
    _metaTvKind :: Kind,
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
  pPrint (MetaTv _ _ rigidName _) = text rigidName

instance HasKind MetaTv where
  kind (MetaTv _ k _ _) = k

instance Store MetaTv where
  size = error "Store MetaTv"
  poke _ = error "Store MetaTv"
  peek = error "Store MetaTv"

---------------------------
-- Read and Write MetaTv --
---------------------------

newMetaTv :: (MonadUniq f, MonadIO f) => Kind -> String -> f MetaTv
newMetaTv k rigitName = MetaTv <$> getUniq <*> pure k <*> pure rigitName <*> newIORef Nothing

readMetaTv :: MonadIO m => MetaTv -> m (Maybe Type)
readMetaTv (MetaTv _ _ _ ref) = readIORef ref

writeMetaTv :: MonadIO m => MetaTv -> Type -> m ()
writeMetaTv (MetaTv _ k _ ref) t
  | k == kind t = writeIORef ref (Just t)
  | otherwise = errorDoc $ "Panic!" <+> "Kind of" <+> pPrint t <+> "is not" <+> pPrint k

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
zonkType t = pure t

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
  kind (TyApp t _) = case kind t of
    (KArr _ k) -> k
    _ -> error "invalid kind"
  kind (TyVar t) = kind t
  kind (TyCon c) = kind c
  kind (TyPrim _) = Type Boxed -- FIXME: 適切なRepを定義する
  kind (TyArr _ _) = Type Boxed
  kind (TyTuple _) = Type Boxed
  kind (TyLazy _) = Type Boxed
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
  pPrintPrec _ _ (TyMeta tv) = pPrint tv

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
