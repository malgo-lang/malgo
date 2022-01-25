{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.TypeRep where

import Control.Lens (At (at), Lens', Plated (plate), Traversal', coerced, cosmos, makeLenses, makePrisms, over, toListOf, transform, traverseOf, view, (^.), _1)
import Data.Binary (Binary)
import Data.Data (Data)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import Koriel.Id
import Koriel.Pretty
import Malgo.Prelude

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
  deriving stock (Eq, Ord, Show, Generic, Data)

instance Binary Rep

instance Pretty Rep where pPrint rep = text $ show rep

-- | Primitive Types
data PrimT = Int32T | Int64T | FloatT | DoubleT | CharT | StringT
  deriving stock (Eq, Show, Ord, Generic, Data)

instance Binary PrimT

instance Pretty PrimT where
  pPrint Int32T = "Int32#"
  pPrint Int64T = "Int64#"
  pPrint FloatT = "Float#"
  pPrint DoubleT = "Double#"
  pPrint CharT = "Char#"
  pPrint StringT = "String#"

--------------------------
-- Type representations --
--------------------------

type Kind = Type

-- | Definition of `Type`
data Type
  = -- type level operator

    -- | application of type constructor
    TyApp Type (NonEmpty Type)
  | -- | type variable (qualified by `Forall`)
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
  | -- | pointer type
    TyPtr Type
  | -- | bottom type
    TyBottom
  | -- kind constructor

    -- | star
    TYPE Type
  | -- | kind of runtime representation tags
    TyRep
  | -- | runtime representation tag
    Rep Rep
  | -- unifiable type variable

    -- | type variable (not qualified)
    TyMeta TypeVar
  deriving stock (Eq, Ord, Show, Generic, Data)

instance Binary Type

instance Plated Type where
  plate f = \case
    t@TyMeta {} -> pure t
    TyApp c ts -> TyApp <$> f c <*> traverse f ts
    TyVar x -> TyVar <$> traverseOf idMeta f x
    TyCon x -> TyCon <$> traverseOf idMeta f x
    t@TyPrim {} -> pure t
    TyArr t1 t2 -> TyArr <$> f t1 <*> f t2
    t@TyTuple {} -> pure t
    TyRecord kts -> TyRecord <$> traverse f kts
    TyPtr t -> TyPtr <$> f t
    t@TyBottom -> pure t
    TYPE t -> TYPE <$> f t
    t@TyRep -> pure t
    t@Rep {} -> pure t

-- plate f t = uniplate f t

instance Pretty Type where
  -- pPrintPrec l d (TyApp (TyCon c) ts) = maybeParens (d > 10) $ foldl' (<+>) (pPrintPrec l 0 c) (map (pPrintPrec l 11) ts)
  pPrintPrec l _ (TyApp (TyTuple _) ts) = parens $ sep $ punctuate "," $ toList $ fmap (pPrintPrec l 0) ts
  pPrintPrec l d (TyApp t ts) = maybeParens (d > 10) $ foldl' (<+>) (pPrintPrec l 0 t) $ toList $ fmap (pPrintPrec l 11) ts
  pPrintPrec _ _ (TyVar v) = pPrint v
  pPrintPrec l _ (TyCon c) = pPrintPrec l 0 c
  pPrintPrec l _ (TyPrim p) = pPrintPrec l 0 p
  pPrintPrec l d (TyArr t1 t2) =
    maybeParens (d > 10) $ pPrintPrec l 11 t1 <+> "->" <+> pPrintPrec l 10 t2
  pPrintPrec _ _ (TyTuple n) = parens $ sep $ replicate (max 0 (n - 1)) ","
  pPrintPrec l _ (TyRecord kvs) = braces $ sep $ punctuate "," $ map (\(k, v) -> pPrintPrec l 0 k <> ":" <+> pPrintPrec l 0 v) $ Map.toList kvs
  pPrintPrec l d (TyPtr ty) = maybeParens (d > 10) $ sep ["Ptr#", pPrintPrec l 11 ty]
  pPrintPrec _ _ TyBottom = "#Bottom"
  pPrintPrec l _ (TYPE rep) = "TYPE" <+> pPrintPrec l 0 rep
  pPrintPrec _ _ TyRep = "#Rep"
  pPrintPrec l _ (Rep rep) = pPrintPrec l 0 rep
  pPrintPrec l _ (TyMeta tv) = pPrintPrec l 0 tv

-------------------
-- Type variable --
-------------------

newtype TypeVar = TypeVar {_typeVar :: Id Kind}
  deriving newtype (Eq, Ord, Show, Generic, Hashable)
  deriving stock (Data, Typeable)

instance Binary TypeVar

instance Pretty TypeVar where
  pPrint (TypeVar v) = "'" <> pPrint v

instance HasType TypeVar where
  typeOf = TyMeta
  types f (TypeVar v) = TypeVar <$> traverseOf idMeta f v

instance HasKind TypeVar where
  kindOf = view (typeVar . idMeta)

typeVar :: Lens' TypeVar (Id Type)
typeVar = coerced

-------------------------
-- HasType and HasKind --
-------------------------

-- | Types that have a `Type`
class HasType a where
  typeOf :: a -> Type
  types :: Traversal' a Type

class HasKind a where
  kindOf :: a -> Kind

instance HasType t => HasType (Annotated t a) where
  typeOf (Annotated x _) = typeOf x
  types f (Annotated x a) = Annotated <$> traverseOf types f x <*> pure a

instance HasKind PrimT where
  kindOf Int32T = TYPE (Rep Int32Rep)
  kindOf Int64T = TYPE (Rep Int64Rep)
  kindOf FloatT = TYPE (Rep FloatRep)
  kindOf DoubleT = TYPE (Rep DoubleRep)
  kindOf CharT = TYPE (Rep CharRep)
  kindOf StringT = TYPE (Rep StringRep)

instance HasType Type where
  typeOf = identity
  types = identity

instance HasKind Type where
  -- kindOf (TyApp (kindOf -> TyArr _ k) _) = k
  -- kindOf TyApp {} = error "invalid kind"
  kindOf (TyApp (kindOf -> k) (toList -> ts)) = apply k ts
    where
      apply k [] = k
      apply (TyArr _ k) (_ : ts) = apply k ts
      apply _ _ = error "invalid kind"
  kindOf (TyVar v) = v ^. idMeta
  kindOf (TyCon c) = c ^. idMeta
  kindOf (TyPrim p) = kindOf p
  kindOf (TyArr _ t2) = kindOf t2
  kindOf (TyTuple n) = buildTyArr (replicate n $ TYPE (Rep BoxedRep)) (TYPE (Rep BoxedRep))
  kindOf (TyRecord _) = TYPE (Rep BoxedRep)
  kindOf (TyPtr _) = TYPE (Rep BoxedRep)
  kindOf TyBottom = TYPE (Rep BoxedRep)
  kindOf (TYPE rep) = TYPE rep -- Type :: Type
  kindOf TyRep = TyRep -- Rep :: Rep
  kindOf (Rep _) = TyRep
  kindOf (TyMeta tv) = kindOf tv

instance HasType Void where
  typeOf x = absurd x
  types _ = absurd

instance HasKind Void where
  kindOf = absurd

-- | Universally quantified type
data Scheme ty = Forall [Id ty] ty
  deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

instance Binary ty => Binary (Scheme ty)

instance Pretty ty => Pretty (Scheme ty) where
  pPrint (Forall vs t) = "forall" <+> hsep (map pPrint vs) <> "." <+> pPrint t

-- | Types qualified with `Type`
class WithType a where
  withType :: Lens' a Type

instance WithType (Annotated Type a) where
  withType = ann

instance WithType Void where
  withType _ a = absurd a

-- | Definition of Type constructor
-- valueConstructorsのSchemeは、typeParametersで全称化されている
data TypeDef ty = TypeDef
  { _typeConstructor :: ty,
    _typeParameters :: [Id ty],
    _valueConstructors :: [(Id (), Scheme ty)]
  }
  deriving stock (Show, Generic, Functor, Foldable, Traversable)

instance Binary ty => Binary (TypeDef ty)

instance Pretty ty => Pretty (TypeDef ty) where
  pPrint (TypeDef c q u) = pPrint (c, q, u)

instance HasKind ty => HasKind (TypeDef ty) where
  kindOf TypeDef {_typeConstructor} = kindOf _typeConstructor

-----------------------
-- Unification monad --
-----------------------

type TypeMap = HashMap TypeVar Type

newtype TypeUnifyT m a = TypeUnifyT {unTypeUnifyT :: StateT TypeMap m a}
  deriving newtype (Functor, Applicative, Monad, MonadReader r, MonadIO, MonadFail)

instance MonadState s m => MonadState s (TypeUnifyT m) where
  get = TypeUnifyT $ lift get
  put x = TypeUnifyT $ lift $ put x

instance MonadTrans TypeUnifyT where
  lift m = TypeUnifyT $ lift m

runTypeUnifyT :: Monad m => TypeUnifyT m a -> m a
runTypeUnifyT (TypeUnifyT m) = evalStateT m mempty

---------------
-- Utilities --
---------------

buildTyApp :: Type -> [Type] -> Type
buildTyApp t [] = t
buildTyApp t (x:xs) = TyApp t (x :| xs)

buildTyArr :: Foldable t => t Type -> Type -> Type
buildTyArr ps ret = foldr TyArr ret ps

-- | split a function type into its parameter types and return type
splitTyArr :: Type -> ([Type], Type)
splitTyArr (TyArr t1 t2) = over _1 (t1 :) $ splitTyArr t2
splitTyArr t = ([], t)

-- | apply substitution to a type
applySubst :: HashMap (Id Type) Type -> Type -> Type
applySubst subst = transform $ \case
  TyVar v -> fromMaybe (TyVar v) $ subst ^. at v
  ty -> ty

-- | expand type synonyms
expandTypeSynonym :: HashMap (Id Kind) ([Id Kind], Type) -> Type -> Maybe Type
expandTypeSynonym abbrEnv (TyApp (TyCon con) ts) =
  case abbrEnv ^. at con of
    Nothing -> Nothing
    Just (ps, orig) -> Just (applySubst (HashMap.fromList $ zip ps $ toList ts) orig)
expandTypeSynonym abbrEnv (TyCon con) =
  case abbrEnv ^. at con of
    Nothing -> Nothing
    Just (_, orig) ->
      -- 引数が与えられていないので、ここでは代入は行わない
      -- 必要ならばunifyで適切な型が代入される
      Just orig
expandTypeSynonym _ _ = Nothing

expandAllTypeSynonym :: HashMap (Id Kind) ([Id Kind], Type) -> Type -> Type
expandAllTypeSynonym abbrEnv (TyApp (TyCon con) ts) =
  case abbrEnv ^. at con of
    Nothing -> TyApp (TyCon con) $ fmap (expandAllTypeSynonym abbrEnv) ts
    Just (ps, orig) ->
      -- ネストした型シノニムを展開するため、展開直後の型をもう一度展開する
      expandAllTypeSynonym abbrEnv $ applySubst (HashMap.fromList $ zip ps $ toList ts) $ expandAllTypeSynonym abbrEnv orig
expandAllTypeSynonym abbrEnv (TyCon con) =
  case abbrEnv ^. at con of
    Nothing -> TyCon con
    Just (_, orig) ->
      -- ネストした型シノニムを展開するため、展開直後の型をもう一度展開する
      expandAllTypeSynonym abbrEnv orig
expandAllTypeSynonym abbrEnv (TyApp c ts) = TyApp (expandAllTypeSynonym abbrEnv c) (fmap (expandAllTypeSynonym abbrEnv) ts)
expandAllTypeSynonym _ t@TyVar {} = t
expandAllTypeSynonym _ t@TyPrim {} = t
expandAllTypeSynonym abbrEnv (TyArr t1 t2) = TyArr (expandAllTypeSynonym abbrEnv t1) (expandAllTypeSynonym abbrEnv t2)
expandAllTypeSynonym _ t@TyTuple {} = t
expandAllTypeSynonym abbrEnv (TyRecord kts) = TyRecord $ fmap (expandAllTypeSynonym abbrEnv) kts
expandAllTypeSynonym abbrEnv (TyPtr t) = TyPtr $ expandAllTypeSynonym abbrEnv t
expandAllTypeSynonym _ TyBottom = TyBottom
expandAllTypeSynonym abbrEnv (TYPE rep) = TYPE $ expandAllTypeSynonym abbrEnv rep
expandAllTypeSynonym _ t@TyRep {} = t
expandAllTypeSynonym _ t@Rep {} = t
expandAllTypeSynonym _ (TyMeta tv) = TyMeta tv

makePrisms ''Rep
makePrisms ''PrimT
makePrisms ''Type
makePrisms ''Scheme
makeLenses ''TypeDef

freevars :: Type -> HashSet TypeVar
freevars ty = HashSet.fromList $ toListOf (cosmos . _TyMeta) ty
