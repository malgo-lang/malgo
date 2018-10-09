{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.Type
  ( HasType(..)
  , matchType
  , TyRef(..)
  , TypeScheme(..)
  , Type(..)
  , TyCon(..)
  )
where

import           Data.List (nub)
import           Prelude   (show)
import           Universum hiding (Type)

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

data TypeScheme a = Forall [a] (Type a)
  deriving Show

newtype TyRef a = TyRef (IORef (Maybe (Type a)))
  deriving Eq

instance Show (TyRef a) where
  show _ = "<meta>"

data Type a = TyApp (TyCon a) [Type a]
            | TyVar a
            | TyMeta (TyRef a)
  deriving (Eq, Show)

data TyCon a = IntC Integer
             | Float32C
             | Float64C
             | ArrayC
             | ArrowC
             | RecordC [Text]
             | VariantC [Text]
             | TyFun [a] (Type a)
  deriving (Eq, Show)

instance HasType (Type a) where
  type Env (Type a) = ()
  type TypeRep (Type a) = Type a
  typeOf = return
