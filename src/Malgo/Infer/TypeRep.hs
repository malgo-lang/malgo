{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Infer.TypeRep
  ( PrimT (..),
    Kind,
    TypeVar,
    KindCtx,
    insertKind,
    askKind,
    Type (..),
    MetaVar (..),
    HasType (..),
    HasKind (..),
    Scheme (..),
    TypeDef (..),
    typeConstructor,
    typeParameters,
    valueConstructors,
    TypeUnifyT (..),
    runTypeUnifyT,
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

import Control.Lens (At (at), Traversal', makeLenses, mapped, over, (^.), _1, _2)
import Data.Binary (Binary)
import Data.Binary.Instances.UnorderedContainers ()
import Data.Data (Data)
import Data.HashMap.Strict qualified as HashMap
import Koriel.Id
import Koriel.Pretty
import Malgo.Prelude

--------------------------------
-- Common tag representations --
--------------------------------

-- | Primitive Types
data PrimT = Int32T | Int64T | FloatT | DoubleT | CharT | StringT
  deriving stock (Eq, Show, Ord, Generic, Data)
  deriving anyclass (Hashable, Binary)

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

type TypeVar = Id ()

-- TODO: Add insert function that ignores `hoge = TYPE`
type KindCtx = HashMap TypeVar Kind

insertKind :: TypeVar -> Kind -> KindCtx -> KindCtx
insertKind tv k ctx
  | k == TYPE = ctx
  | otherwise = HashMap.insert tv k ctx

askKind :: TypeVar -> KindCtx -> Kind
askKind tv ctx = fromMaybe TYPE (HashMap.lookup tv ctx)

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
    TyRecord (HashMap Text Type)
  | -- | pointer type
    TyPtr
  | -- kind constructor

    -- | star
    TYPE
  | -- unifiable type variable

    -- | type variable (not qualified)
    TyMeta MetaVar
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (Hashable, Binary)

instance Pretty Type where
  pPrintPrec l _ (TyConApp (TyCon c) ts) = foldl' (<+>) (pPrintPrec l 0 c) (map (pPrintPrec l 11) ts)
  pPrintPrec l _ (TyConApp (TyTuple _) ts) = parens $ sep $ punctuate "," $ map (pPrintPrec l 0) ts
  pPrintPrec l _ (TyConApp TyPtr [t]) = "Ptr#" <+> pPrintPrec l 0 t
  pPrintPrec l d (TyApp t1 t2) =
    maybeParens (d > 10) $ hsep [pPrintPrec l 10 t1, pPrintPrec l 11 t2]
  pPrintPrec _ _ (TyVar v) = pPrint v
  pPrintPrec l _ (TyCon c) = pPrintPrec l 0 c
  pPrintPrec l _ (TyPrim p) = pPrintPrec l 0 p
  pPrintPrec l d (TyArr t1 t2) =
    maybeParens (d > 10) $ pPrintPrec l 11 t1 <+> "->" <+> pPrintPrec l 10 t2
  pPrintPrec _ _ (TyTuple n) = parens $ sep $ replicate (max 0 (n - 1)) ","
  pPrintPrec l _ (TyRecord kvs) = braces $ sep $ punctuate "," $ map (\(k, v) -> pPrintPrec l 0 k <> ":" <+> pPrintPrec l 0 v) $ HashMap.toList kvs
  pPrintPrec _ _ TyPtr = "Ptr#"
  pPrintPrec _ _ TYPE = "TYPE"
  pPrintPrec l _ (TyMeta tv) = pPrintPrec l 0 tv

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

buildTyArr :: Foldable t => t Type -> Type -> Type
buildTyArr ps ret = foldr TyArr ret ps

-- | split a function type into its parameter types and return type
splitTyArr :: Type -> ([Type], Type)
splitTyArr (TyArr t1 t2) = over _1 (t1 :) $ splitTyArr t2
splitTyArr t = ([], t)

-------------------
-- Type variable --
-------------------

newtype MetaVar = MetaVar {metaVar :: Id ()}
  deriving newtype (Eq, Ord, Show, Generic, Hashable)
  deriving stock (Data, Typeable)
  deriving anyclass (Binary)

instance Pretty MetaVar where
  pPrint (MetaVar v) = "'" <> pPrint v

instance HasType MetaVar where
  typeOf = TyMeta
  types _ (MetaVar v) = pure $ MetaVar v

instance HasKind MetaVar where
  kindOf ctx MetaVar {metaVar} = askKind metaVar ctx

-------------------------
-- HasType and HasKind --
-------------------------

-- | Types that have a `Type`
class HasType a where
  typeOf :: a -> Type
  types :: Traversal' a Type

class HasKind a where
  kindOf :: KindCtx -> a -> Kind

instance HasKind TypeVar where
  kindOf ctx v = askKind v ctx

instance HasKind PrimT where
  kindOf _ _ = TYPE

instance HasType Type where
  typeOf = identity
  types = identity

instance HasKind Type where
  kindOf ctx (TyApp (kindOf ctx -> TyArr _ k) _) = k
  kindOf _ TyApp {} = error "invalid kind"
  kindOf ctx (TyVar v) = kindOf ctx v
  kindOf ctx (TyCon c) = kindOf ctx c
  kindOf ctx (TyPrim p) = kindOf ctx p
  kindOf ctx (TyArr _ t2) = kindOf ctx t2
  kindOf _ (TyTuple n) = buildTyArr (replicate n TYPE) TYPE
  kindOf _ (TyRecord _) = TYPE
  kindOf _ TyPtr = TYPE `TyArr` TYPE
  kindOf _ TYPE = TYPE -- Type :: Type
  kindOf ctx (TyMeta tv) = kindOf ctx tv

instance HasType Void where
  typeOf = absurd
  types _ = absurd

instance HasKind Void where
  kindOf _ = absurd

-- | Universally quantified type
data Scheme ty = Forall [TypeVar] ty
  deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (Hashable, Binary)

instance Pretty ty => Pretty (Scheme ty) where
  pPrint (Forall [] t) = pPrint t
  pPrint (Forall vs t) = "forall" <+> hsep (map pPrint vs) <> "." <+> pPrint t

-- | Definition of Type constructor
-- valueConstructorsのSchemeは、typeParametersで全称化されている
data TypeDef ty = TypeDef
  { _typeConstructor :: ty,
    _typeParameters :: [TypeVar],
    _valueConstructors :: [(Id (), Scheme ty)]
  }
  deriving stock (Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (Binary)

instance Pretty ty => Pretty (TypeDef ty) where
  pPrint (TypeDef c q u) = pPrint (c, q, u)

instance HasKind ty => HasKind (TypeDef ty) where
  kindOf ctx TypeDef {_typeConstructor} = kindOf ctx _typeConstructor

makeLenses ''TypeDef

-----------------------
-- Unification monad --
-----------------------

type TypeMap = HashMap MetaVar Type

-- | Note for The Instance of 'MonadState' for 'TypeUnifyT':
--
-- @MonadState TypeUnifyT@ does not use 'TypeMap'.
-- Instead, it uses inner monad's 'MonadState' instance.
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

-- | apply substitution to a type
applySubst :: HashMap TypeVar Type -> Type -> Type
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
expandTypeSynonym :: HashMap TypeVar ([TypeVar], Type) -> Type -> Maybe Type
expandTypeSynonym abbrEnv (TyConApp (TyCon con) ts) =
  case abbrEnv ^. at con of
    Nothing -> Nothing
    Just (ps, orig) -> Just (applySubst (HashMap.fromList $ zip ps ts) orig)
expandTypeSynonym _ _ = Nothing

expandAllTypeSynonym :: HashMap TypeVar ([TypeVar], Type) -> Type -> Type
expandAllTypeSynonym abbrEnv (TyConApp (TyCon con) ts) =
  case abbrEnv ^. at con of
    Nothing -> TyConApp (TyCon con) $ map (expandAllTypeSynonym abbrEnv) ts
    Just (ps, orig) ->
      -- ネストした型シノニムを展開するため、展開直後の型をもう一度展開する
      expandAllTypeSynonym abbrEnv $ applySubst (HashMap.fromList $ zip ps ts) $ expandAllTypeSynonym abbrEnv orig
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
freevars :: KindCtx -> Type -> HashSet MetaVar
freevars ctx (TyApp t1 t2) = freevars ctx t1 <> freevars ctx t2
freevars ctx v@TyVar {} = freevars ctx $ kindOf ctx v
freevars ctx c@TyCon {} = freevars ctx $ kindOf ctx c
freevars _ TyPrim {} = mempty
freevars ctx (TyArr t1 t2) = freevars ctx t1 <> freevars ctx t2
freevars _ TyTuple {} = mempty
freevars ctx (TyRecord kts) = foldMap (freevars ctx) kts
freevars _ TyPtr = mempty
freevars _ TYPE = mempty
freevars _ (TyMeta tv) = one tv
