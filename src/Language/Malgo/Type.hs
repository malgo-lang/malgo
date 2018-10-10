{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.Type
  ( kind
  , TypeScheme(..)
  , Kind(..)
  , TyRef(..)
  , Type(..)
  , TyCon(..)
  , TypeId(..)
  , tInt
  , tFloat
  , tArrow
  , fn
  )
where

import           Language.Malgo.Id
import           Prelude           (show)
import           Universum         hiding (Type)

newtype TyRef = TyRef (IORef (Maybe Type))
  deriving Eq

instance Show TyRef where
  show _ = "<meta>"

data Kind = Type
          | KFun Kind Kind
  deriving (Eq, Ord, Show)

data TypeId = TypeId Id Kind
  deriving (Eq, Ord, Show)

data TypeScheme = Forall [TypeId] Type
  deriving (Eq, Show)

data Type = TyApp Type Type
          | TyVar TypeId
          | TyCon TyCon Kind
          | TyMeta TyRef Kind
  deriving (Eq, Show)

kind :: Type -> Kind
kind (TyApp t _) =
  case kind t of
    (KFun _ k) -> k
    _          -> error "unreachable(kind)"
kind (TyVar (TypeId _ k)) = k
kind (TyCon _ k) = k
kind (TyMeta _ k) = k

data TyCon = IntC Integer
           | Float32C
           | Float64C
           | ArrayC
           | ArrowC
           | RecordC [Text]
           | VariantC [Text]
           | TyFun [TypeId] Type
  deriving (Eq, Show)

tInt :: Type
tInt = TyCon (IntC 64) Type
tFloat :: Type
tFloat = TyCon Float32C Type
tArrow :: Type
tArrow = TyCon ArrowC (KFun Type (KFun Type Type))

fn :: Type -> Type -> Type
a `fn` b = TyApp (TyApp tArrow a) b
