{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
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
  deriving (Eq, Show)

data TyCon a = IntC Integer
             | Float32C
             | Float64C
             | TupleC Integer
             | ArrayC
             | ArrowC
             | RecordC [Field a]
             | TyFun [a] (Type a)
             | Unique Integer (TyCon a)
  deriving (Eq, Show)

type Field a = (Text, Type a)

instance HasType (Type a) where
  type Env (Type a) = ()
  type TypeRep (Type a) = Type a
  typeOf = return
