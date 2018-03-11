{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.TypedID
  ( TypedID(..)
  ) where

import           Language.Malgo.ID
import           Language.Malgo.Prelude
import           Language.Malgo.Type

data TypedID = TypedID {_id :: ID, _type :: Type}
    deriving (Show, Ord, Read)

instance Eq TypedID where
    (TypedID x _) == (TypedID y _) = x == y

instance PrettyPrint TypedID where
    pretty (TypedID x t) = pretty x <> ":" <> pretty t

instance Typeable TypedID where
    typeOf (TypedID _ t) = t
