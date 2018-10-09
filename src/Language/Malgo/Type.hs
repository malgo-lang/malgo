{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.Type
  ( HasType(..)
  , matchType
  , kind
  , TypeScheme(..)
  , TyRef(..)
  , Type(..)
  , TypeId(..)
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
          | Kind :-> Kind
  deriving (Eq, Show)

data TypeId = TypeId Id Kind
  deriving (Eq, Show)

data TypeScheme = TypeScheme [TypeId] Type
  deriving (Eq, Show)

data Type = TyApp Type Type
          | TyVar TypeId
          | TyCon TyCon Kind
          | TyMeta TyRef Kind
  deriving (Eq, Show)

kind :: Type -> Kind
kind (TyApp t _) =
  case kind t of
    (_ :-> k) -> k
    _         -> error "unreachable(kind)"
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
