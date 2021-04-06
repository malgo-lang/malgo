{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.TypeRep.Static where

import Data.Binary (Binary)
import Data.Fix
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as Text
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

type Kind = Type

-- | Definition of Type constructor
data TyAbs ty
  = Abs (Id ty) (TyAbs ty)
  | Type ty
  deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

instance Binary ty => Binary (TyAbs ty)

instance Pretty ty => Pretty (TyAbs ty) where
  pPrint (Abs x t) = braces $ pPrint x <+> "->" <+> pPrint t
  pPrint (Type t) = pPrint t

-- | Definition of `Type`
data Type
  = -- type level operator

    -- | application of type constructor
    TyApp Type Type
  | -- | type variable
    TyVar (Id Kind)
  | -- | type constructor
    TyCon (Id Kind)
  | TyAbs (TyAbs Type)
  | -- primitive type constructor

    -- | primitive types
    TyPrim PrimT
  | -- | function type
    TyArr Type Type
  | -- | tuple type
    TyTuple [Type]
  | -- record type
    TyRecord (Map Text Type)
  | -- | lazy type
    TyLazy Type
  | -- | pointer type
    TyPtr Type
  | -- kind constructor

    -- | star
    TYPE Type
  | -- | kind of runtime representation tags
    TyRep
  | -- | runtime representation tag
    Rep Rep
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

makePrisms ''Type
makeBaseFunctor ''Type

instance Pretty Type where
  pPrintPrec l d (TyApp t1 t2) =
    maybeParens (d > 10) $ sep [pPrintPrec l 10 t1, pPrintPrec l 11 t2]
  pPrintPrec _ _ (TyVar v) = pprIdName v
  pPrintPrec l _ (TyCon c) = pPrintPrec l 0 c
  pPrintPrec l _ (TyAbs a) = pPrintPrec l 0 a
  pPrintPrec l _ (TyPrim p) = pPrintPrec l 0 p
  pPrintPrec l d (TyArr t1 t2) =
    maybeParens (d > 10) $ pPrintPrec l 11 t1 <+> "->" <+> pPrintPrec l 10 t2
  pPrintPrec l _ (TyTuple ts) = parens $ sep $ punctuate "," $ map (pPrintPrec l 0) ts
  pPrintPrec l _ (TyRecord kvs) = braces $ sep $ punctuate "," $ map (\(k, v) -> text (Text.unpack k) <> ":" <+> pPrintPrec l 0 v) $ Map.toList kvs
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
  typeOf :: Monad m => a -> m Type

-- HasType (Id a)は混乱を生む
-- instance IsType a => HasType (Id a) where
--   typeOf x = pure $ toType $ x ^. idMeta

instance HasType PrimT where
  typeOf Int32T = pure $ TYPE (Rep Int32Rep)
  typeOf Int64T = pure $ TYPE (Rep Int64Rep)
  typeOf FloatT = pure $ TYPE (Rep FloatRep)
  typeOf DoubleT = pure $ TYPE (Rep DoubleRep)
  typeOf CharT = pure $ TYPE (Rep CharRep)
  typeOf StringT = pure $ TYPE (Rep StringRep)

instance (HasType ty) => HasType (TyAbs ty) where
  typeOf (Abs x t) = do
    xKind <- typeOf $ x ^. idMeta
    tKind <- typeOf t
    pure $ TyArr xKind tKind
  typeOf (Type t) = typeOf t

instance HasType Type where
  typeOf (TyApp t1 _) =
    typeOf t1 >>= \case
      TyArr _ k -> pure k
      _ -> error "invalid kind"
  typeOf (TyVar v) = pure $ v ^. idMeta
  typeOf (TyCon c) = pure $ c ^. idMeta
  typeOf (TyAbs a) = typeOf a
  typeOf (TyPrim p) = typeOf p
  typeOf (TyArr _ t2) = typeOf t2
  typeOf (TyTuple _) = pure $ TYPE (Rep BoxedRep)
  typeOf (TyRecord _) = pure $ TYPE (Rep BoxedRep)
  typeOf (TyLazy _) = pure $ TYPE (Rep BoxedRep)
  typeOf (TyPtr _) = pure $ TYPE (Rep BoxedRep)
  typeOf (TYPE rep) = pure $ TYPE rep -- Type :: Type
  typeOf TyRep = pure TyRep -- Rep :: Rep
  typeOf (Rep _) = pure TyRep

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

data TypeDef ty = TypeDef
  { _definedType :: TyAbs ty,
    _valueConstructors :: [(Id (), ty)]
  }
  deriving stock (Show, Generic)

instance Binary ty => Binary (TypeDef ty)

instance Pretty ty => Pretty (TypeDef ty) where
  pPrint (TypeDef t v) = pPrint (t, v)

makeLenses ''TypeDef

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
applySubst subst (TyAbs abs) = TyAbs $ applySubstAbs abs
  where
    applySubstAbs (Abs x t) = Abs x $ applySubstAbs t
    applySubstAbs (Type t) = Type $ applySubst subst t
applySubst _ (TyPrim p) = TyPrim p
applySubst subst (TyArr t1 t2) = TyArr (applySubst subst t1) (applySubst subst t2)
applySubst subst (TyTuple ts) = TyTuple $ map (applySubst subst) ts
applySubst subst (TyRecord kvs) = TyRecord $ fmap (applySubst subst) kvs
applySubst subst (TyLazy t) = TyLazy $ applySubst subst t
applySubst subst (TyPtr t) = TyPtr $ applySubst subst t
applySubst subst (TYPE rep) = TYPE $ applySubst subst rep
applySubst _ TyRep = TyRep
applySubst _ (Rep rep) = Rep rep
