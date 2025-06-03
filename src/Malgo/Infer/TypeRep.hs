{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Malgo.Infer.TypeRep
  ( PrimT (..),
    Kind,
    TypeVar,
    Type (..),
    MetaVar (..),
    HasType (..),
    Typed (..),
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

import Control.Lens (Traversal', lens, mapped, _1, _2)
import Data.Data (Data)
import Data.Map.Strict qualified as Map (fromList, lookup, toList)
import Data.Set qualified as Set (singleton)
import Data.Store (Store)
import Effectful (Eff)
import Effectful.State.Static.Local (State, evalState)
import Malgo.Id
import Malgo.Prelude
import Malgo.Syntax hiding (Type (..))
import Malgo.Syntax.Extension
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

data Type
  = -- | `TyApp` is an application of type constructor.
    TyApp Type Type
  | -- | `TyVar` is a type variable (qualified by `Forall`).
    TyVar TypeVar
  | -- | `TyCon` is a user-defined type constructor.
    TyCon TypeVar
  | -- | `TyPrim` is a primitive type constructor.
    TyPrim PrimT
  | -- | `TyArr` is a function type constructor.
    TyArr Type Type
  | -- | `TyTuple` is a tuple type constructor.
    TyTuple Int
  | -- | `TyRecord` is a record type constructor.
    -- It is also used for record kinds (like `forall (a : {X : b}) b. a -> b`).
    TyRecord (Map Text Type)
  | -- | `TyPtr` is a pointer type.
    TyPtr
  | -- | `TYPE` is the basic kind of types.
    -- Regular types are of kind `TYPE`, and type constructors are of kind `TYPE -> TYPE`.
    TYPE
  | -- | `TyMeta` is a meta type variable used for unification.
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
  deriving stock (Data)
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
  TyVar id -> fromMaybe (TyVar id) $ Map.lookup id subst
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
  case Map.lookup con abbrEnv of
    Nothing -> Nothing
    Just (ps, orig) -> Just (applySubst (Map.fromList $ zip ps ts) orig)
expandTypeSynonym _ _ = Nothing

expandAllTypeSynonym :: Map TypeVar ([TypeVar], Type) -> Type -> Type
expandAllTypeSynonym abbrEnv (TyConApp (TyCon con) ts) =
  case Map.lookup con abbrEnv of
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

instance HasType (Literal x) where
  typeOf Int32 {} = TyPrim Int32T
  typeOf Int64 {} = TyPrim Int64T
  typeOf Float {} = TyPrim FloatT
  typeOf Double {} = TyPrim DoubleT
  typeOf Char {} = TyPrim CharT
  typeOf String {} = TyPrim StringT
  types f v = f (typeOf v) $> v

instance
  (ForallExpX HasType x, ForallClauseX HasType x, ForallPatX HasType x) =>
  HasType (Expr x)
  where
  typeOf (Var x _) = typeOf x
  typeOf (Unboxed x _) = typeOf x
  typeOf (Boxed x _) = typeOf x
  typeOf (Apply x _ _) = typeOf x
  typeOf (OpApp x _ _ _) = typeOf x
  typeOf (Project x _ _) = typeOf x
  typeOf (Fn x _) = typeOf x
  typeOf (Tuple x _) = typeOf x
  typeOf (Record x _) = typeOf x
  typeOf (List x _) = typeOf x
  typeOf (Ann x _ _) = typeOf x
  typeOf (Seq x _) = typeOf x
  typeOf (Parens x _) = typeOf x

  types f = \case
    Var x v -> Var <$> types f x <*> pure v
    Unboxed x u -> Unboxed <$> types f x <*> types f u
    Boxed x b -> Boxed <$> types f x <*> types f b
    Apply x e1 e2 -> Apply <$> types f x <*> types f e1 <*> types f e2
    OpApp x op e1 e2 -> OpApp <$> types f x <*> pure op <*> types f e1 <*> types f e2
    Project x e k -> Project <$> types f x <*> types f e <*> pure k
    Fn x cs -> Fn <$> types f x <*> traverse (types f) cs
    Tuple x es -> Tuple <$> types f x <*> traverse (types f) es
    Record x kvs -> Record <$> types f x <*> traverse (\(k, v) -> (k,) <$> types f v) kvs
    List x es -> List <$> types f x <*> traverse (types f) es
    Ann x e t -> Ann <$> types f x <*> types f e <*> pure t
    Seq x ss -> Seq <$> types f x <*> traverse (types f) ss
    Parens x e -> Parens <$> types f x <*> types f e

instance
  (ForallExpX HasType x, ForallClauseX HasType x, ForallPatX HasType x) =>
  HasType (Stmt x)
  where
  typeOf (Let _ _ e) = typeOf e
  typeOf (With _ _ e) = typeOf e
  typeOf (NoBind _ e) = typeOf e

  types f = \case
    Let x v e -> Let x v <$> types f e
    With x v e -> With x v <$> types f e
    NoBind x e -> NoBind x <$> types f e

instance
  (ForallClauseX HasType x, ForallPatX HasType x, ForallExpX HasType x) =>
  HasType (Clause x)
  where
  typeOf (Clause x _ _) = typeOf x

  types f (Clause x ps e) = Clause <$> types f x <*> traverse (types f) ps <*> types f e

instance
  (ForallPatX HasType x) =>
  HasType (Pat x)
  where
  typeOf (VarP x _) = typeOf x
  typeOf (ConP x _ _) = typeOf x
  typeOf (TupleP x _) = typeOf x
  typeOf (RecordP x _) = typeOf x
  typeOf (ListP x _) = typeOf x
  typeOf (UnboxedP x _) = typeOf x
  typeOf (BoxedP x _) = typeOf x

  types f = \case
    VarP x v -> VarP <$> types f x <*> pure v
    ConP x c ps -> ConP <$> types f x <*> pure c <*> traverse (types f) ps
    TupleP x ps -> TupleP <$> types f x <*> traverse (types f) ps
    RecordP x kps -> RecordP <$> types f x <*> traverse (bitraverse pure (types f)) kps
    ListP x ps -> ListP <$> types f x <*> traverse (types f) ps
    UnboxedP x u -> UnboxedP <$> types f x <*> types f u
    BoxedP x b -> BoxedP <$> types f x <*> types f b

data Typed x = Typed {annotated :: Type, value :: x}
  deriving stock (Eq, Ord, Show)

instance (Pretty x) => Pretty (Typed x) where
  pretty (Typed t v) = pretty v <+> ":" <+> pretty t

instance HasType (Typed x) where
  typeOf (Typed t _) = t
  types = lens (.annotated) (\x y -> x {annotated = y})

type instance SimpleX 'Infer = Typed (SimpleX 'Rename)

type instance XOpApp (Malgo 'Infer) = Typed (XOpApp (Malgo 'Rename))

type instance XForeign (Malgo 'Infer) = Typed (XForeign (Malgo 'Rename))
