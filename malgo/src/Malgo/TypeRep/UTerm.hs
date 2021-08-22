{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Malgo.TypeRep.UTerm where

import Data.Deriving
import Data.Functor.Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Void
import Koriel.Id
import Koriel.Pretty
import Malgo.Infer.UTerm
import Malgo.Prelude
import Malgo.TypeRep.Static (IsType (fromType, safeToType), Rep (..), TypeF (..))
import qualified Malgo.TypeRep.Static as S

----------
-- Type --
----------

deriveEq1 ''TypeF
deriveOrd1 ''TypeF
deriveShow1 ''TypeF

type UType = UTerm TypeF TypeVar

instance Pretty t => Pretty (TypeF t) where
  pPrintPrec l d (TyAppF t1 t2) =
    maybeParens (d > 10) $ hsep [pPrintPrec l 10 t1, pPrintPrec l 11 t2]
  pPrintPrec _ _ (TyVarF v) = pprIdName v
  pPrintPrec l d (TyConF c) = pPrintPrec l d c
  pPrintPrec _ _ (TyPrimF p) = pPrint p
  pPrintPrec l d (TyArrF t1 t2) =
    maybeParens (d > 10) $ pPrintPrec l 11 t1 <+> "->" <+> pPrintPrec l 10 t2
  pPrintPrec _ _ (TyTupleF n) = parens $ sep $ replicate (max 0 (n - 1)) ","
  pPrintPrec l _ (TyRecordF kvs) = braces $ sep $ punctuate "," $ map (\(k, v) -> pPrintPrec l 0 k <> ":" <+> pPrintPrec l 0 v) $ Map.toList kvs
  pPrintPrec _ _ TyLazyF = "{}"
  pPrintPrec l d (TyPtrF t) = maybeParens (d > 10) $ sep ["Ptr#", pPrintPrec l 11 t]
  pPrintPrec _ _ TyBottomF = "#Bottom"
  pPrintPrec l _ (TYPEF rep) = "TYPE" <+> pPrintPrec l 0 rep
  pPrintPrec _ _ TyRepF = "#Rep"
  pPrintPrec l _ (RepF rep) = pPrintPrec l 0 rep

instance (IsType a) => IsType (TypeF a) where
  safeToType = fmap embed . traverse safeToType
  fromType = fmap fromType . project

class HasType a where
  typeOf :: a -> UType
  types :: Traversal' a UType

class HasKind a where
  kindOf :: a -> UType

instance HasType t => HasType (With t a) where
  typeOf (With x _) = typeOf x
  types f (With x a) = With <$> traverseOf types f x <*> pure a

newtype TypeVar = TypeVar {_typeVar :: Id UType}
  deriving newtype (Eq, Ord, Show, Generic, Hashable)
  deriving stock (Data, Typeable)

makeLenses ''TypeVar

instance HasType TypeVar where
  typeOf tv = UVar tv
  types f (TypeVar x) = TypeVar <$> traverseOf idMeta f x

instance Pretty TypeVar where
  pPrint (TypeVar v) = "'" <> pPrint v

type TypeMap = HashMap TypeVar UType

newtype TypeUnifyT m a = TypeUnifyT {unTypeUnifyT :: StateT TypeMap m a}
  deriving newtype (Functor, Applicative, Monad, MonadReader r, MonadIO, MonadFail)

instance MonadState s m => MonadState s (TypeUnifyT m) where
  get = TypeUnifyT $ lift get
  put x = TypeUnifyT $ lift $ put x

instance MonadTrans TypeUnifyT where
  lift m = TypeUnifyT $ lift m

runTypeUnifyT :: Monad m => TypeUnifyT m a -> m a
runTypeUnifyT (TypeUnifyT m) = evalStateT m mempty

applySubst :: [(Id UType, UType)] -> UType -> UType
applySubst subst t =
  transform ?? t $ \case
    TyVar v -> fromMaybe (TyVar v) $ List.lookup v subst
    t -> t

instance HasType UType where
  typeOf = id
  types = id 

instance HasKind UType where
  kindOf (UVar v) = v ^. typeVar . idMeta
  kindOf (UTerm t) = case t of
    TyAppF (kindOf -> TyArr _ k) _ -> k
    TyAppF _ _ -> error "invalid kind"
    TyVarF v -> v ^. idMeta
    TyConF c -> c ^. idMeta
    TyPrimF p -> S.fromType $ S.kindOf p
    TyArrF _ t2 -> kindOf t2
    TyTupleF n -> buildTyArr (replicate n $ TYPE (Rep BoxedRep)) (TYPE (Rep BoxedRep))
    TyRecordF _ -> TYPE (Rep BoxedRep)
    TyLazyF -> TyArr (TYPE (Rep BoxedRep)) (TYPE (Rep BoxedRep))
    TyPtrF _ -> TYPE (Rep BoxedRep)
    TyBottomF -> TYPE (Rep BoxedRep)
    TYPEF rep -> TYPE rep
    TyRepF -> TyRep
    RepF _ -> TyRep

instance HasType Void where
  typeOf = absurd
  types _ = absurd

instance HasKind Void where
  kindOf = absurd

pattern TyApp :: UTerm TypeF v -> UTerm TypeF v -> UTerm TypeF v
pattern TyApp t1 t2 = UTerm (TyAppF t1 t2)

pattern TyVar :: Id (UTerm TypeF v) -> UTerm TypeF v
pattern TyVar v = UTerm (TyVarF v)

pattern TyCon :: Id (UTerm TypeF v) -> UTerm TypeF v
pattern TyCon c = UTerm (TyConF c)

pattern TyPrim :: S.PrimT -> UTerm TypeF v
pattern TyPrim p = UTerm (TyPrimF p)

pattern TyArr :: UTerm TypeF v -> UTerm TypeF v -> UTerm TypeF v
pattern TyArr t1 t2 = UTerm (TyArrF t1 t2)

pattern TyTuple :: Int -> UTerm TypeF v
pattern TyTuple n = UTerm (TyTupleF n)

pattern TyRecord :: Map.Map (Id ()) (UTerm TypeF v) -> UTerm TypeF v
pattern TyRecord kts = UTerm (TyRecordF kts)

pattern TyLazy :: UTerm TypeF v
pattern TyLazy = UTerm TyLazyF

pattern TyPtr :: UTerm TypeF v -> UTerm TypeF v
pattern TyPtr t = UTerm (TyPtrF t)

pattern TYPE :: UTerm TypeF v -> UTerm TypeF v
pattern TYPE rep = UTerm (TYPEF rep)

pattern TyRep :: UTerm TypeF v
pattern TyRep = UTerm TyRepF

pattern Rep :: Rep -> UTerm TypeF v
pattern Rep rep = UTerm (RepF rep)

---------------
-- Utilities --
---------------

pattern TyConApp :: UType -> [UType] -> UType
pattern TyConApp x xs <-
  (viewTyConApp -> Just (x, xs))
  where
    TyConApp x xs = buildTyApp x xs

buildTyApp :: UType -> [UType] -> UType
buildTyApp = List.foldl TyApp

buildTyArr :: [UType] -> UType -> UType
buildTyArr ps ret = foldr TyArr ret ps

viewTyConApp :: UType -> Maybe (UType, [UType])
viewTyConApp (TyCon con) = Just (TyCon con, [])
viewTyConApp (TyTuple n) = Just (TyTuple n, [])
viewTyConApp TyLazy = Just (TyLazy, [])
viewTyConApp (TyApp t1 t2) = over (mapped . _2) (<> [t2]) $ viewTyConApp t1
viewTyConApp _ = Nothing

splitTyArr :: UType -> ([UType], UType)
splitTyArr (TyArr t1 t2) = let (ps, r) = splitTyArr t2 in (t1 : ps, r)
splitTyArr t = ([], t)

expandTypeSynonym :: HashMap (Id UType) ([Id UType], UType) -> UType -> Maybe UType
expandTypeSynonym abbrEnv (TyConApp (TyCon con) ts) =
  case abbrEnv ^. at con of
    Nothing -> Nothing
    Just (ps, orig) -> Just (applySubst (zip ps ts) orig)
expandTypeSynonym _ _ = Nothing

expandAllTypeSynonym :: HashMap (Id UType) ([Id UType], UType) -> UType -> UType
expandAllTypeSynonym _ (UVar v) = UVar v
expandAllTypeSynonym abbrEnv (TyConApp (TyCon con) ts) =
  case abbrEnv ^. at con of
    Nothing -> TyConApp (TyCon con) $ map (expandAllTypeSynonym abbrEnv) ts
    Just (ps, orig) ->
      -- ネストした型シノニムを展開するため、展開直後の型をもう一度展開する
      expandAllTypeSynonym abbrEnv $ applySubst (zip ps ts) $ expandAllTypeSynonym abbrEnv orig
expandAllTypeSynonym abbrEnv (TyApp t1 t2) = TyApp (expandAllTypeSynonym abbrEnv t1) (expandAllTypeSynonym abbrEnv t2)
expandAllTypeSynonym _ t@TyVar {} = t
expandAllTypeSynonym _ t@TyCon {} = t
expandAllTypeSynonym _ t@TyPrim {} = t
expandAllTypeSynonym abbrEnv (TyArr t1 t2) = TyArr (expandAllTypeSynonym abbrEnv t1) (expandAllTypeSynonym abbrEnv t2)
expandAllTypeSynonym _ t@TyTuple {} = t
expandAllTypeSynonym abbrEnv (TyRecord kts) = TyRecord $ fmap (expandAllTypeSynonym abbrEnv) kts
expandAllTypeSynonym _ t@TyLazy {} = t
expandAllTypeSynonym abbrEnv (TyPtr t) = TyPtr $ expandAllTypeSynonym abbrEnv t
expandAllTypeSynonym abbrEnv (TYPE rep) = TYPE $ expandAllTypeSynonym abbrEnv rep
expandAllTypeSynonym _ t@TyRep {} = t
expandAllTypeSynonym _ t@Rep {} = t
expandAllTypeSynonym _ UTerm {} = bug $ Unreachable "All patterns are covered"
