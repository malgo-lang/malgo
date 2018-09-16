{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.Type where

import           Universum hiding (Type)

class HasType a where
  type Env a :: *
  type TypeRep a :: *
  typeOf :: MonadReader (Env a) m => a -> m (TypeRep a)

matchType
  :: ( HasType a, HasType b
     , Eq (TypeRep a), MonadReader (Env a) m
     , Env a ~ Env b, TypeRep a ~ TypeRep b)
  => a -> b -> m Bool
matchType x y = (==) <$> typeOf x <*> typeOf y

data TypeScheme a = Forall [a] (Type a)
  deriving (Eq, Show)

data Type a = TyApp (TyCon a) [Type a]
            | TyVar a
  deriving (Eq, Show, Ord)

data TyCon a = IntC Integer
             | Float32C
             | Float64C
             | TupleC Integer
             | ArrayC
             | ArrowC
             | RecordC (Field a)
             | VariantC (Field a)
             | TyFun [a] (Type a)
             | Unique Integer (TyCon a)
  deriving (Eq, Show, Ord)

newtype Field a = Field [(Text, Type a)]
  deriving Show

instance Ord a => Eq (Field a) where
  (Field x) == (Field y) = sort x == sort y

instance Ord a => Ord (Field a) where
  compare (Field x) (Field y) = compare (sort x) (sort y)

instance HasType (Type a) where
  type Env (Type a) = ()
  type TypeRep (Type a) = Type a
  typeOf = return
