{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.TypeRep.Static where

import Data.Binary (Binary)
import Data.Fix
import Data.Maybe (fromJust, fromMaybe)
import Data.Void
import Koriel.Id
import Koriel.Pretty
import Language.Malgo.Prelude

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

-- | Definition of `Type`
data Type
  = -- | application of type constructor
    TyApp Type Type
  | -- | type variable
    TyVar (Id Type)
  | -- | type constructor
    TyCon (Id Type)
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
  | -- | kind
    TYPE Type
  | -- | type of runtime representation tags
    TyRep
  | -- | runtime representation tag
    Rep Rep
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

makePrisms ''Type

instance Pretty Type where
  pPrintPrec l d (TyApp t1 t2) =
    maybeParens (d > 10) $ sep [pPrintPrec l 10 t1, pPrintPrec l 11 t2]
  pPrintPrec _ _ (TyVar v) = pprIdName v
  pPrintPrec l _ (TyCon c) = pPrintPrec l 0 c
  pPrintPrec l _ (TyPrim p) = pPrintPrec l 0 p
  pPrintPrec l d (TyArr t1 t2) =
    maybeParens (d > 10) $ pPrintPrec l 11 t1 <+> "->" <+> pPrintPrec l 10 t2
  pPrintPrec l _ (TyTuple ts) = parens $ sep $ punctuate "," $ map (pPrintPrec l 0) ts
  pPrintPrec l _ (TyLazy t) = braces $ pPrintPrec l 0 t
  pPrintPrec l d (TyPtr t) = maybeParens (d > 10) $ sep ["Ptr#", pPrintPrec l 11 t]
  pPrintPrec l _ (TYPE rep) = pPrintPrec l 0 rep
  pPrintPrec _ _ TyRep = "#Rep"
  pPrintPrec l _ (Rep rep) = pPrintPrec l 0 rep

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

instance HasType PrimT where
  typeOf Int32T = TYPE (Rep Int32Rep)
  typeOf Int64T = TYPE (Rep Int64Rep)
  typeOf FloatT = TYPE (Rep FloatRep)
  typeOf DoubleT = TYPE (Rep DoubleRep)
  typeOf CharT = TYPE (Rep CharRep)
  typeOf StringT = TYPE (Rep StringRep)

instance HasType Type where
  typeOf (TyApp t1 _) = case typeOf t1 of
    TyArr _ k -> k
    _ -> error "invalid kind"
  typeOf (TyVar v) = v ^. idMeta
  typeOf (TyCon c) = c ^. idMeta
  typeOf (TyPrim p) = typeOf p
  typeOf (TyArr _ t2) = typeOf t2
  typeOf (TyTuple _) = TYPE (Rep BoxedRep)
  typeOf (TyLazy _) = TYPE (Rep BoxedRep)
  typeOf (TyPtr _) = TYPE (Rep BoxedRep)
  typeOf (TYPE rep) = TYPE rep -- Type :: Type
  typeOf TyRep = TyRep -- Rep :: Rep
  typeOf (Rep _) = TyRep

instance HasType Void where
  typeOf x = absurd x

-- | Universally quantified type
data Scheme = Forall [Id Type] Type
  deriving stock (Show, Generic)

instance Binary Scheme

instance Pretty Scheme where
  pPrint (Forall vs t) = "forall" <+> sep (map pprIdName vs) <> "." <+> pPrint t

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
class WithType a where
  withType :: Lens' a Type

instance WithType (With Type a) where
  withType = ann

instance WithType Void where
  withType _ a = absurd a

-- | Definition of Type constructor
data TypeDef = TypeDef
  { _typeConstructor :: Type,
    _typeParameters :: [Id Type],
    _valueConstructors :: [(Id (), Type)]
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

splitCon :: Type -> (Id Type, [Type])
splitCon (TyCon con) = (con, [])
splitCon (TyApp t1 t2) = over _2 (<> [t2]) $ splitCon t1
splitCon _ = bug Unreachable

splitTyArr :: Type -> ([Type], Type)
splitTyArr (TyArr t1 t2) = over _1 (t1 :) $ splitTyArr t2
splitTyArr t = ([], t)

applySubst :: HashMap (Id Type) Type -> Type -> Type
applySubst subst (TyApp t1 t2) = TyApp (applySubst subst t1) (applySubst subst t2)
applySubst subst (TyVar v) = fromMaybe (TyVar v) $ subst ^. at v
applySubst _ (TyCon c) = TyCon c
applySubst _ (TyPrim p) = TyPrim p
applySubst subst (TyArr t1 t2) = TyArr (applySubst subst t1) (applySubst subst t2)
applySubst subst (TyTuple ts) = TyTuple $ map (applySubst subst) ts
applySubst subst (TyLazy t) = TyLazy $ applySubst subst t
applySubst subst (TyPtr t) = TyPtr $ applySubst subst t
applySubst subst (TYPE rep) = TYPE $ applySubst subst rep
applySubst _ TyRep = TyRep
applySubst _ (Rep rep) = Rep rep
