{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Infer.TypeRep
  ( PrimT (..),
    Kind,
    TypeVar,
    Type (..),
    MetaVar (..),
    HasType (..),
    Scheme (..),
    TypeDef (..),
    TypeMap,
    runTypeUnify,
    pattern TyConApp,
    viewTyConApp,
    buildTyArr,
    splitTyArr,
    applySubst,
    expandTypeSynonym,
    expandAllTypeSynonym,
    freevars,
  )
where

import Control.Lens (At (at), Traversal', mapped, (^.), _1, _2)
import Data.Data (Data)
import Data.Map.Strict qualified as Map (fromList, toList)
import Data.Set qualified as Set (singleton)
import Data.Store (Store)
import Effectful (Eff)
import Effectful.State.Static.Local (State, evalState)
import Malgo.Id
import Malgo.Prelude
import Prettyprinter (braces, hsep, parens, punctuate, sep, (<+>))

--------------------------------
-- Common tag representations --
--------------------------------

-- | Primitive Types
data PrimT = Int32T | Int64T | FloatT | DoubleT | CharT | StringT
  deriving stock (Eq, Show, Ord, Generic, Data)
  deriving anyclass (Hashable, Store)

instance Pretty PrimT where
  pretty Int32T = "Int32#"
  pretty Int64T = "Int64#"
  pretty FloatT = "Float#"
  pretty DoubleT = "Double#"
  pretty CharT = "Char#"
  pretty StringT = "String#"

--------------------------
-- Type representations --
--------------------------

type Kind = Type

type TypeVar = Id

-- | Definition of `Type`
data Type
  = -- type level operator

    -- | application of type constructor
    TyApp Type Type
  | -- | type variable (qualified by `Forall`)
    TyVar TypeVar
  | -- | type constructor
    TyCon TypeVar
  | -- primitive type constructor

    -- | primitive types
    TyPrim PrimT
  | -- | function type
    TyArr Type Type
  | -- | tuple type
    TyTuple Int
  | -- record type
    TyRecord (Map Text Type)
  | -- | pointer type
    TyPtr
  | -- kind constructor

    -- | star
    TYPE
  | -- unifiable type variable

    -- | type variable (not qualified)
    TyMeta MetaVar
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (Hashable, Store)

instance Pretty Type where
  pretty = prettyPrec 0
    where
      prettyPrec :: Int -> Type -> Doc ann
      prettyPrec _ (TyConApp (TyCon c) ts) = foldl' (<+>) (pretty c) (map (prettyPrec 11) ts)
      prettyPrec _ (TyConApp (TyTuple _) ts) = parens $ sep $ punctuate "," $ map (prettyPrec 0) ts
      prettyPrec _ (TyConApp TyPtr [t]) = "Ptr#" <+> prettyPrec 0 t
      prettyPrec d (TyApp t1 t2) =
        maybeParens (d > 10) $ hsep [prettyPrec 10 t1, prettyPrec 11 t2]
      prettyPrec _ (TyVar v) = pretty v
      prettyPrec _ (TyCon c) = pretty c
      prettyPrec _ (TyPrim p) = pretty p
      prettyPrec d (TyArr t1 t2) =
        maybeParens (d > 10) $ prettyPrec 11 t1 <+> "->" <+> prettyPrec 10 t2
      prettyPrec _ (TyTuple n) = parens $ sep $ replicate (max 0 (n - 1)) ","
      prettyPrec _ (TyRecord kvs) = braces $ sep $ punctuate "," $ map (\(k, v) -> pretty k <> ":" <+> pretty v) $ Map.toList kvs
      prettyPrec _ TyPtr = "Ptr#"
      prettyPrec _ TYPE = "TYPE"
      prettyPrec _ (TyMeta tv) = pretty tv

pattern TyConApp :: Type -> [Type] -> Type
pattern TyConApp x xs <-
  (viewTyConApp -> Just (x, xs))
  where
    TyConApp x xs = buildTyApp x xs

viewTyConApp :: Type -> Maybe (Type, [Type])
viewTyConApp (TyCon con) = Just (TyCon con, [])
viewTyConApp (TyTuple n) = Just (TyTuple n, [])
viewTyConApp TyPtr = Just (TyPtr, [])
viewTyConApp (TyApp t1 t2) = over (mapped . _2) (<> [t2]) $ viewTyConApp t1
viewTyConApp _ = Nothing

buildTyApp :: Type -> [Type] -> Type
buildTyApp = foldl' TyApp

buildTyArr :: (Foldable t) => t Type -> Type -> Type
buildTyArr ps ret = foldr TyArr ret ps

-- | split a function type into its parameter types and return type
splitTyArr :: Type -> ([Type], Type)
splitTyArr (TyArr t1 t2) = over _1 (t1 :) $ splitTyArr t2
splitTyArr t = ([], t)

-------------------
-- Type variable --
-------------------

newtype MetaVar = MetaVar {metaVar :: Id}
  deriving newtype (Eq, Ord, Show, Generic, Hashable)
  deriving stock (Data, Typeable)
  deriving anyclass (Store)

instance Pretty MetaVar where
  pretty (MetaVar v) = "'" <> pretty v

instance HasType MetaVar where
  typeOf = TyMeta
  types _ (MetaVar v) = pure $ MetaVar v

-------------------------
-- HasType and HasKind --
-------------------------

-- | Types that have a `Type`
class HasType a where
  typeOf :: a -> Type
  types :: Traversal' a Type

instance HasType Type where
  typeOf = identity
  types = identity

instance HasType Void where
  typeOf = absurd
  types _ = absurd

-- | Universally quantified type
data Scheme ty = Forall [TypeVar] ty
  deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (Hashable, Store)

instance (Pretty ty) => Pretty (Scheme ty) where
  pretty (Forall [] t) = pretty t
  pretty (Forall vs t) = "forall" <+> hsep (map pretty vs) <> "." <+> pretty t

-- | Definition of Type constructor
-- valueConstructorsのSchemeは、typeParametersで全称化されている
data TypeDef ty = TypeDef
  { typeConstructor :: ty,
    typeParameters :: [TypeVar],
    valueConstructors :: [(Id, Scheme ty)]
  }
  deriving stock (Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (Store)

instance (Pretty ty) => Pretty (TypeDef ty) where
  pretty (TypeDef c q u) = pretty (c, q, u)

-----------------------
-- Unification monad --
-----------------------

type TypeMap = Map MetaVar Type

runTypeUnify :: Eff (State TypeMap : es) a -> Eff es a
runTypeUnify = evalState mempty

---------------
-- Utilities --
---------------

-- | apply substitution to a type
applySubst :: Map TypeVar Type -> Type -> Type
applySubst subst = \case
  TyApp ty ty' -> TyApp (applySubst subst ty) (applySubst subst ty')
  TyVar id -> fromMaybe (TyVar id) $ subst ^. at id
  TyCon id -> TyCon id
  TyPrim pt -> TyPrim pt
  TyArr ty ty' -> TyArr (applySubst subst ty) (applySubst subst ty')
  TyTuple n -> TyTuple n
  TyRecord hm -> TyRecord $ fmap (applySubst subst) hm
  TyPtr -> TyPtr
  TYPE -> TYPE
  TyMeta tv -> TyMeta tv

-- | expand type synonyms
expandTypeSynonym :: Map TypeVar ([TypeVar], Type) -> Type -> Maybe Type
expandTypeSynonym abbrEnv (TyConApp (TyCon con) ts) =
  case abbrEnv ^. at con of
    Nothing -> Nothing
    Just (ps, orig) -> Just (applySubst (Map.fromList $ zip ps ts) orig)
expandTypeSynonym _ _ = Nothing

expandAllTypeSynonym :: Map TypeVar ([TypeVar], Type) -> Type -> Type
expandAllTypeSynonym abbrEnv (TyConApp (TyCon con) ts) =
  case abbrEnv ^. at con of
    Nothing -> TyConApp (TyCon con) $ map (expandAllTypeSynonym abbrEnv) ts
    Just (ps, orig) ->
      -- ネストした型シノニムを展開するため、展開直後の型をもう一度展開する
      expandAllTypeSynonym abbrEnv $ applySubst (Map.fromList $ zip ps ts) $ expandAllTypeSynonym abbrEnv orig
expandAllTypeSynonym abbrEnv (TyApp t1 t2) = TyApp (expandAllTypeSynonym abbrEnv t1) (expandAllTypeSynonym abbrEnv t2)
expandAllTypeSynonym _ t@TyVar {} = t
expandAllTypeSynonym _ t@TyCon {} = t
expandAllTypeSynonym _ t@TyPrim {} = t
expandAllTypeSynonym abbrEnv (TyArr t1 t2) = TyArr (expandAllTypeSynonym abbrEnv t1) (expandAllTypeSynonym abbrEnv t2)
expandAllTypeSynonym _ t@TyTuple {} = t
expandAllTypeSynonym abbrEnv (TyRecord kts) = TyRecord $ fmap (expandAllTypeSynonym abbrEnv) kts
expandAllTypeSynonym _ TyPtr = TyPtr
expandAllTypeSynonym _ TYPE = TYPE
expandAllTypeSynonym _ (TyMeta tv) = TyMeta tv

-- | get all meta type variables in a type
freevars :: Type -> Set MetaVar
freevars (TyApp t1 t2) = freevars t1 <> freevars t2
freevars TyVar {} = mempty
freevars TyCon {} = mempty
freevars TyPrim {} = mempty
freevars (TyArr t1 t2) = freevars t1 <> freevars t2
freevars TyTuple {} = mempty
freevars (TyRecord kts) = foldMap freevars kts
freevars TyPtr = mempty
freevars TYPE = mempty
freevars (TyMeta tv) = Set.singleton tv
