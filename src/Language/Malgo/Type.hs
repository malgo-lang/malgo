{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.Type
  ( HasType(..)
  , matchType
  , TypeScheme(..)
  , TyRef(..)
  , Type(..)
  )
where

import           Language.Malgo.Id
import           Prelude           (show)
import           Universum         hiding (Type)

class HasType a where
  type Env a :: *
  type TypeRep a :: *
  typeOf :: MonadReader (Env a) m => a -> m (TypeRep a)

matchType
  :: ( HasType a
     , HasType b
     , Eq (TypeRep a)
     , MonadReader (Env a) m
     , Env a ~ Env b
     , TypeRep a ~ TypeRep b
     )
  => a
  -> b
  -> m Bool
matchType x y = (==) <$> typeOf x <*> typeOf y

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

data Type = TyApp Type [Type]
          | TyVar TypeId
          | TyCon TyCon Kind
          | TyMeta TyRef
  deriving (Eq, Show)

data TyCon = IntC Integer
           | Float32C
           | Float64C
           | ArrayC
           | ArrowC
           | RecordC [Text]
           | VariantC [Text]
           -- -- | TyFun [TypeId] Type -- TODO: TyFunの意義
  deriving (Eq, Show)

instance HasType Type where
  type Env Type = ()
  type TypeRep Type = Type
  typeOf = return
