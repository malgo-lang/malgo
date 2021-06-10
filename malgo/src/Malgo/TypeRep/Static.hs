{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.TypeRep.Static where

import Data.Binary (Binary)
import Data.Fix
import Data.Functor.Foldable.TH (makeBaseFunctor)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Void
import Koriel.Id
import Koriel.Pretty
import Malgo.Prelude
import Malgo.UTerm

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

-- | Definition of `Type`
data Type
  = -- type level operator

    -- | application of type constructor
    TyApp Type Type
  | -- | type variable
    TyVar (Id Kind)
  | -- | type constructor
    TyCon (Id Kind)
  | -- primitive type constructor

    -- | primitive types
    TyPrim PrimT
  | -- | function type
    TyArr Type Type
  | -- | tuple type
    TyTuple Int
  | -- record type
    TyRecord (Map (Id ()) Type)
  | -- | lazy type
    TyLazy
  | -- | pointer type
    TyPtr Kind
  | -- | bottom type
    TyBottom
  | -- kind constructor

    -- | star
    TYPE Type
  | -- | kind of runtime representation tags
    TyRep
  | -- | runtime representation tag
    Rep Rep
  deriving stock (Eq, Ord, Show, Generic)

instance Binary Type

makePrisms ''Type
makeBaseFunctor ''Type

instance Pretty Type where
  pPrintPrec l d (TyApp t1 t2) =
    maybeParens (d > 10) $ hsep [pPrintPrec l 10 t1, pPrintPrec l 11 t2]
  pPrintPrec _ _ (TyVar v) = pprIdName v
  pPrintPrec l _ (TyCon c) = pPrintPrec l 0 c
  pPrintPrec l _ (TyPrim p) = pPrintPrec l 0 p
  pPrintPrec l d (TyArr t1 t2) =
    maybeParens (d > 10) $ pPrintPrec l 11 t1 <+> "->" <+> pPrintPrec l 10 t2
  pPrintPrec _ _ (TyTuple n) = parens $ sep $ replicate (max 0 (n - 1)) ","
  pPrintPrec l _ (TyRecord kvs) = braces $ sep $ punctuate "," $ map (\(k, v) -> pPrintPrec l 0 k <> ":" <+> pPrintPrec l 0 v) $ Map.toList kvs
  pPrintPrec _ _ TyLazy = "{}"
  pPrintPrec _ _ (TyPtr _) = "Ptr#"
  pPrintPrec _ _ TyBottom = "#Bottom"
  pPrintPrec l _ (TYPE rep) = pPrintPrec l 0 rep
  pPrintPrec _ _ TyRep = "#Rep"
  pPrintPrec l _ (Rep rep) = pPrintPrec l 0 rep

-- | Types that can be translated to `Type`
class IsType a where
  _Type :: Prism' a Type
  _Type = prism' fromType safeToType
  safeToType :: a -> Maybe Type
  safeToType a = a ^? _Type
  toType :: HasCallStack => a -> Type
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

instance IsType (t (UTerm t v)) => IsType (UTerm t v) where
  safeToType (UVar _) = Just TyBottom -- TODO: return TypeVar
  safeToType (UTerm t) = safeToType t
  fromType t = UTerm $ fromType t

-- | Types that have a `Type`
class HasType a where
  typeOf :: Monad m => a -> m Type

class HasKind a where
  kindOf :: Monad m => a -> m Kind

instance HasKind PrimT where
  kindOf Int32T = pure $ TYPE (Rep Int32Rep)
  kindOf Int64T = pure $ TYPE (Rep Int64Rep)
  kindOf FloatT = pure $ TYPE (Rep FloatRep)
  kindOf DoubleT = pure $ TYPE (Rep DoubleRep)
  kindOf CharT = pure $ TYPE (Rep CharRep)
  kindOf StringT = pure $ TYPE (Rep StringRep)

instance HasKind Type where
  kindOf (TyApp t1 _) =
    kindOf t1 >>= \case
      TyArr _ k -> pure k
      _ -> error "invalid kind"
  kindOf (TyVar v) = pure $ v ^. idMeta
  kindOf (TyCon c) = pure $ c ^. idMeta
  kindOf (TyPrim p) = kindOf p
  kindOf (TyArr _ t2) = kindOf t2
  kindOf (TyTuple _) = pure $ TYPE (Rep BoxedRep)
  kindOf (TyRecord _) = pure $ TYPE (Rep BoxedRep)
  kindOf TyLazy = pure $ TYPE (Rep BoxedRep) `TyArr` TYPE (Rep BoxedRep)
  kindOf (TyPtr k) = pure $ k `TyArr` TYPE (Rep BoxedRep)
  kindOf TyBottom = pure $ TYPE (Rep BoxedRep)
  kindOf (TYPE rep) = pure $ TYPE rep -- Type :: Type
  kindOf TyRep = pure TyRep -- Rep :: Rep
  kindOf (Rep _) = pure TyRep

instance HasType Void where
  typeOf x = absurd x

instance HasKind Void where
  kindOf x = absurd x

-- | Universally quantified type
data Scheme ty = Forall [Id ty] ty
  deriving stock (Show, Generic, Functor, Foldable, Traversable)

instance Binary ty => Binary (Scheme ty)

instance Pretty ty => Pretty (Scheme ty) where
  pPrint (Forall vs t) = "forall" <+> hsep (map pprIdName vs) <> "." <+> pPrint t

makePrisms ''Scheme

-- | Types qualified with `Type`
class WithType a where
  withType :: Lens' a Type

instance WithType (With Type a) where
  withType = ann

instance WithType Void where
  withType _ a = absurd a

-- | Definition of Type constructor
data TypeDef ty
  = TypeDef
      { _typeConstructor :: ty,
        _typeParameters :: [Id ty],
        _valueConstructors :: [(Id (), Scheme ty)]
      }
  deriving stock (Show, Generic, Functor, Foldable, Traversable)

instance Binary ty => Binary (TypeDef ty)

instance Pretty ty => Pretty (TypeDef ty) where
  pPrint (TypeDef c q u) = pPrint (c, q, u)

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
applySubst _ (TyPrim p) = TyPrim p
applySubst subst (TyArr t1 t2) = TyArr (applySubst subst t1) (applySubst subst t2)
applySubst _ (TyTuple n) = TyTuple n
applySubst subst (TyRecord kvs) = TyRecord $ fmap (applySubst subst) kvs
applySubst _ TyLazy = TyLazy
applySubst subst (TyPtr k) = TyPtr $ applySubst subst k
applySubst _ TyBottom = TyBottom
applySubst subst (TYPE rep) = TYPE $ applySubst subst rep
applySubst _ TyRep = TyRep
applySubst _ (Rep rep) = Rep rep
