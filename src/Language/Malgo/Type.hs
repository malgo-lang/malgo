{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.Type where

import           Universum

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
