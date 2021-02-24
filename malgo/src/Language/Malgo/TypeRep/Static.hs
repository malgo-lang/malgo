{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.TypeRep.Static where

import Data.Binary (Binary)
import Data.Fix
import qualified Data.Map as Map
import Koriel.Id
import Koriel.Prelude
import Koriel.Pretty
import {-# SOURCE #-} Language.Malgo.Syntax.Extension (ModuleName)

--------------------------------
-- Common tag representations --
--------------------------------

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

makePrisms ''Rep

instance Binary Rep

instance Pretty Rep where pPrint rep = text $ show rep

-- | Primitive Types
data PrimT = Int32T | Int64T | FloatT | DoubleT | CharT | StringT
  deriving stock (Eq, Show, Ord, Generic)

makePrisms ''PrimT

instance Binary PrimT

instance Pretty PrimT where
  pPrint Int32T = "Int32#"
  pPrint Int64T = "Int64#"
  pPrint FloatT = "Float#"
  pPrint DoubleT = "Double#"
  pPrint CharT = "Char#"
  pPrint StringT = "String#"

----------------------------
-- Static representations --
----------------------------

--- | Definition of `Kind`
data Kind
  = -- | a kind
    TYPE Rep
  | -- | kind arrow
    KArr Kind Kind
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

makePrisms ''Kind

instance Pretty Kind where
  pPrintPrec l _ (TYPE rep) = pPrintPrec l 0 rep
  pPrintPrec l d (KArr k1 k2) =
    maybeParens (d > 10) $ pPrintPrec l 11 k1 <+> "->" <+> pPrintPrec l 10 k2

-- | Types that can be translated to `Kind`
class IsKind a where
  _Kind :: Prism' a Kind
  _Kind = prism' fromKind safeToKind
  safeToKind :: a -> Maybe Kind
  safeToKind a = a ^? _Kind
  toKind :: a -> Kind
  toKind a = fromJust $ safeToKind a
  fromKind :: Kind -> a
  fromKind a = a ^. re _Kind
  {-# MINIMAL _Kind | (safeToKind, fromKind) #-}

instance IsKind Kind where
  _Kind = prism id Right
  safeToKind = Just
  toKind = id

instance IsKind (t (Fix t)) => IsKind (Fix t) where
  safeToKind (Fix a) = safeToKind a
  fromKind k = Fix (fromKind k)

-- | Types that have a `Kind`
class HasKind a where
  kindOf :: a -> Kind

instance HasKind Kind where
  kindOf = id

instance HasKind PrimT where
  kindOf Int32T = TYPE Int32Rep
  kindOf Int64T = TYPE Int64Rep
  kindOf FloatT = TYPE FloatRep
  kindOf DoubleT = TYPE DoubleRep
  kindOf CharT = TYPE CharRep
  kindOf StringT = TYPE StringRep

-- | Definition of `Type`
data Type
  = -- | application of type constructor
    TyApp Type Type
  | -- | type variable
    TyVar (Id Kind)
  | -- | type constructor
    TyCon (Id Kind)
  | -- | primitive types
    TyPrim PrimT
  | -- | function type
    TyArr Type Type
  | -- | tuple type
    TyTuple [Type]
  | -- | lazy type
    TyLazy Type
  | -- | pointer type
    TyPtr Type
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

makePrisms ''Type

instance Pretty Type where
  pPrintPrec l d (TyApp t1 t2) =
    maybeParens (d > 10) $ sep [pPrintPrec l 10 t1, pPrintPrec l 11 t2]
  pPrintPrec l _ (TyVar v) = pPrintPrec l 0 v
  pPrintPrec l _ (TyCon c) = pPrintPrec l 0 c
  pPrintPrec l _ (TyPrim p) = pPrintPrec l 0 p
  pPrintPrec l d (TyArr t1 t2) =
    maybeParens (d > 10) $ pPrintPrec l 11 t1 <+> "->" <+> pPrintPrec l 10 t2
  pPrintPrec l _ (TyTuple ts) = parens $ sep $ punctuate "," $ map (pPrintPrec l 0) ts
  pPrintPrec l _ (TyLazy t) = braces $ pPrintPrec l 0 t
  pPrintPrec l d (TyPtr t) = maybeParens (d > 10) $ sep ["Ptr#", pPrintPrec l 11 t]

instance HasKind Type where
  kindOf (TyApp t1 _) = case kindOf t1 of
    KArr _ k -> k
    _ -> error "invalid kind"
  kindOf (TyVar v) = v ^. idMeta
  kindOf (TyCon c) = c ^. idMeta
  kindOf (TyPrim p) = kindOf p
  kindOf TyArr {} = TYPE BoxedRep
  kindOf TyTuple {} = TYPE BoxedRep
  kindOf TyLazy {} = TYPE BoxedRep
  kindOf TyPtr {} = TYPE BoxedRep

-- | Types that can be translated to `Type`
class IsType a where
  _Type :: Prism' a Type
  _Type = prism' fromType safeToType
  safeToType :: a -> Maybe Type
  safeToType a = a ^? _Type
  toType :: a -> Type
  toType a = fromJust $ safeToType a
  fromType :: Type -> a
  fromType a = a ^. re _Type
  {-# MINIMAL _Type | (safeToType, fromType) #-}

instance IsType Type where
  _Type = prism id Right
  safeToType = Just
  toType = id

instance IsType (t (Fix t)) => IsType (Fix t) where
  safeToType (Fix t) = safeToType t
  fromType t = Fix $ fromType t

-- | Types that have a `Type`
class HasType a where
  typeOf :: a -> Type

instance HasType Type where
  typeOf = id

-- | Universally quantified type
data Scheme = Forall [Id Kind] Type
  deriving stock (Show, Generic)

instance Binary Scheme

instance Pretty Scheme where
  pPrint (Forall vs t) = "forall" <+> sep (map pPrint vs) <> "." <+> pPrint t

makePrisms ''Scheme

-- | Types that can be translated to `Scheme`
class IsScheme a where
  _Scheme :: Prism' a Scheme
  _Scheme = prism' fromScheme safeToScheme
  safeToScheme :: a -> Maybe Scheme
  safeToScheme a = a ^? _Scheme
  toScheme :: a -> Scheme
  toScheme a = fromJust $ safeToScheme a
  fromScheme :: Scheme -> a
  fromScheme a = a ^. re _Scheme
  {-# MINIMAL _Scheme | (safeToScheme, fromScheme) #-}

instance IsScheme Scheme where
  _Scheme = prism id Right
  safeToScheme = Just
  toScheme = id

-- | Types qualified with `Type`
data WithType a = WithType a Type
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Pretty a => Pretty (WithType a) where
  pPrint (WithType a t) = pPrint a <> ":" <> pPrint t

instance HasType (WithType a) where
  typeOf (WithType _ t) = t

-- | Definition of Type constructor
data TypeDef = TypeDef
  { _typeConstructor :: Type,
    _typeParameters :: [Id Kind],
    _valueConstructors :: [(Id ModuleName, Type)]
  }
  deriving stock (Show, Generic)

instance Binary TypeDef

instance Pretty TypeDef where
  pPrint (TypeDef c q u) = pPrint (c, q, u)

makeLenses ''TypeDef

class IsTypeDef a where
  _TypeDef :: Prism' a TypeDef
  _TypeDef = prism' fromTypeDef safeToTypeDef
  safeToTypeDef :: a -> Maybe TypeDef
  safeToTypeDef a = a ^? _TypeDef
  toTypeDef :: a -> TypeDef
  toTypeDef a = fromJust $ safeToTypeDef a
  fromTypeDef :: TypeDef -> a
  fromTypeDef a = a ^. re _TypeDef
  {-# MINIMAL _TypeDef | (safeToTypeDef, fromTypeDef) #-}

---------------
-- Utilities --
---------------

splitCon :: Type -> (Id Kind, [Type])
splitCon (TyCon con) = (con, [])
splitCon (TyApp t1 t2) = over _2 (<> [t2]) $ splitCon t1
splitCon _ = bug Unreachable

splitTyArr :: Type -> ([Type], Type)
splitTyArr (TyArr t1 t2) = over _1 (t1 :) $ splitTyArr t2
splitTyArr t = ([], t)

applySubst :: Map (Id Kind) Type -> Type -> Type
applySubst subst (TyApp t1 t2) = TyApp (applySubst subst t1) (applySubst subst t2)
applySubst subst (TyVar v) = fromMaybe (TyVar v) $ Map.lookup v subst
applySubst _ (TyCon c) = TyCon c
applySubst _ (TyPrim p) = TyPrim p
applySubst subst (TyArr t1 t2) = TyArr (applySubst subst t1) (applySubst subst t2)
applySubst subst (TyTuple ts) = TyTuple $ map (applySubst subst) ts
applySubst subst (TyLazy t) = TyLazy $ applySubst subst t
applySubst subst (TyPtr t) = TyPtr $ applySubst subst t
