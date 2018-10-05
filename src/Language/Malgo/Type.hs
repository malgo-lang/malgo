{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.Type
  ( HasType(..)
  , matchType
  , TypeScheme(..)
  , Type(..)
  , TyCon(..)
  , Field(..)
  )
where

import           Universum               hiding ( Type )

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
  deriving (Eq, Show)

data Type a = TyApp (TyCon a) [Type a]
            | TyVar a
  deriving (Eq, Show, Ord)

data TyCon a = IntC Integer
             | Float32C
             | Float64C
             | ArrayC
             | ArrowC
             | RecordC (Field a)
             | VariantC (Field a)
             | TyFun [a] (Type a)
             | Unique Integer (TyCon a)
  deriving (Eq, Ord, Show)

newtype Field a = Field { getField :: Set (Text, Type a) }
  deriving (Eq, Ord, Show)

instance HasType (Type a) where
  type Env (Type a) = ()
  type TypeRep (Type a) = Type a
  typeOf = return
