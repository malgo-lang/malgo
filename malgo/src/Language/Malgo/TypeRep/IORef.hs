{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.TypeRep.IORef where

import Data.Binary (Binary (..))
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable (hashWithSalt))
import Data.Void
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Pretty
import Language.Malgo.Prelude
import {-# SOURCE #-} Language.Malgo.Syntax.Extension (ModuleName)
import Language.Malgo.TypeRep.Static (IsTypeDef, PrimT (..), Rep (..))
import qualified Language.Malgo.TypeRep.Static as S

----------------------
-- Kind and HasKind --
----------------------

-- | Definition of `kind`
data Kind
  = -- | a kind
    Type Rep
  | -- | kind arrow (* -> *, * is a kind)
    KArr Kind Kind
  deriving stock (Eq, Ord, Show, Generic)

instance Binary Kind

class HasKind a where
  kind :: (MonadIO m) => a -> m (Maybe Kind)

instance HasKind a => HasKind (Id a) where
  kind = kind . view idMeta

instance HasKind Kind where
  kind = pure . Just

instance Pretty Kind where
  pPrintPrec _ _ (Type rep) = pPrint rep
  pPrintPrec l d (KArr k1 k2) =
    maybeParens (d > 10) $ pPrintPrec l 11 k1 <+> "->" <+> pPrintPrec l 10 k2

instance S.IsKind Kind where
  _Kind = prism fromStatic (Right . toStatic)
    where
      fromStatic (S.TYPE rep) = Type rep
      fromStatic (S.KArr k1 k2) = KArr (fromStatic k1) (fromStatic k2)
      toStatic (Type rep) = S.TYPE rep
      toStatic (KArr k1 k2) = S.KArr (toStatic k1) (toStatic k2)

---------------------
-- Primitive Types --
---------------------

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

instance Binary Scheme

instance HasKind Scheme where
  kind (Forall _ t) = kind t

instance Pretty Scheme where
  pPrint (Forall vs t) = "forall" <+> sep (map pPrint vs) <> "." <+> pPrint t

instance S.IsScheme Scheme where
  safeToScheme (Forall vs t) = S.Forall <$> traverse (traverseOf idMeta S.safeToKind) vs <*> S.safeToType t
  fromScheme (S.Forall vs t) = Forall (map (over idMeta S.fromKind) vs) (S.fromType t)

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

instance Binary Type

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
  kind (TyArr _ _) = pure $ Just $ Type BoxedRep
  kind (TyTuple _) = pure $ Just $ Type BoxedRep
  kind (TyLazy _) = pure $ Just $ Type BoxedRep
  kind (TyPtr _) = pure $ Just $ Type BoxedRep
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

instance S.IsType Type where
  safeToType (TyApp t1 t2) = S.TyApp <$> S.safeToType t1 <*> S.safeToType t2
  safeToType (TyVar v) = S.TyVar <$> traverseOf idMeta S.safeToKind v
  safeToType (TyCon c) = S.TyCon <$> traverseOf idMeta S.safeToKind c
  safeToType (TyPrim p) = Just $ S.TyPrim p
  safeToType (TyArr t1 t2) = S.TyArr <$> S.safeToType t1 <*> S.safeToType t2
  safeToType (TyTuple ts) = S.TyTuple <$> traverse S.safeToType ts
  safeToType (TyLazy t) = S.TyLazy <$> S.safeToType t
  safeToType (TyPtr t) = S.TyPtr <$> S.safeToType t
  safeToType TyMeta {} = Nothing
  fromType (S.TyApp t1 t2) = TyApp (S.fromType t1) (S.fromType t2)
  fromType (S.TyVar v) = TyVar (over idMeta (\k -> k ^. re S._Kind) v)
  fromType (S.TyCon c) = TyCon (over idMeta (\k -> k ^. re S._Kind) c)
  fromType (S.TyPrim p) = TyPrim p
  fromType (S.TyArr t1 t2) = TyArr (S.fromType t1) (S.fromType t2)
  fromType (S.TyTuple ts) = TyTuple (map S.fromType ts)
  fromType (S.TyLazy t) = TyLazy (S.fromType t)
  fromType (S.TyPtr t) = TyPtr (S.fromType t)

-------------------
-- Type variable --
-------------------

data MetaTv = MetaTv
  { _metaTvUniq :: Int,
    _metaTvKind :: IORef (Maybe Kind),
    _metaTvRigidName :: String,
    _metaTvTypeRef :: IORef (Maybe Type)
  }

instance Hashable MetaTv where
  hashWithSalt salt MetaTv {_metaTvUniq} = hashWithSalt salt _metaTvUniq

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

instance Binary MetaTv where
  put _ = error "Binary MetaTv"
  get = error "Binary MetaTv"

-- | Definition of type constructor
data TypeDef = TypeDef {_constructor :: Type, _qualVars :: [TyVar], _union :: [(Id ModuleName, Type)]}
  deriving stock (Show, Eq, Generic)

instance Binary TypeDef

instance Pretty TypeDef where
  pPrint (TypeDef c q u) = pPrint (c, q, u)

instance IsTypeDef TypeDef where
  safeToTypeDef TypeDef {_constructor, _qualVars, _union} =
    S.TypeDef <$> S.safeToType _constructor
      <*> traverse (idMeta S.safeToKind) _qualVars
      <*> traverse (_2 S.safeToType) _union
  fromTypeDef S.TypeDef {S._typeConstructor, S._typeParameters, S._valueConstructors} =
    TypeDef (S.fromType _typeConstructor) (map (over idMeta S.fromKind) _typeParameters) (map (over _2 S.fromType) _valueConstructors)

makeLenses ''TypeDef

---------------------------
-- Read and Write MetaTv --
---------------------------

newMetaTv :: (MonadUniq f, MonadIO f) => Maybe Kind -> String -> f MetaTv
newMetaTv k rigidName = MetaTv <$> getUniq <*> newIORef k <*> pure rigidName <*> newIORef Nothing

readMetaTv :: MonadIO m => MetaTv -> m (Maybe Type)
readMetaTv (MetaTv _ _ _ ref) = readIORef ref

writeMetaTv :: (MonadIO m) => MetaTv -> Type -> m ()
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

metaTvs :: Type -> HashSet MetaTv
metaTvs (TyApp t1 t2) = metaTvs t1 <> metaTvs t2
metaTvs (TyArr t1 t2) = metaTvs t1 <> metaTvs t2
metaTvs (TyTuple ts) = mconcat $ map metaTvs ts
metaTvs (TyLazy t) = metaTvs t
metaTvs (TyPtr t) = metaTvs t
metaTvs (TyMeta tv) = HashSet.singleton tv
metaTvs _ = mempty

metaTvsScheme :: Scheme -> HashSet MetaTv
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

instance HasType Scheme where
  toType = to $ \(Forall _ t) -> t
  overType f (Forall vs t) = Forall vs <$> f t

instance HasType a => HasType (Id a) where
  toType = idMeta . toType
  overType f n = traverseOf idMeta (overType f) n

instance HasType Type where
  toType = to id
  overType = id

instance HasType Void where
  toType _ x = absurd x
  overType _ x = absurd x

type WithType a = With Type a

instance HasType t => HasType (With t a) where
  toType = ann . toType
  overType f (With t x) = With <$> overType f t <*> pure x

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
