{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.Old.TypedID
  ( TypedID(..)
  ) where

import           Language.Malgo.Old.ID
import           Language.Malgo.Old.Prelude
import           Language.Malgo.Old.Type

data TypedID = TypedID {_id :: ID, _type :: Type}
    deriving (Show, Ord, Read)

instance Eq TypedID where
    (TypedID x _) == (TypedID y _) = x == y

instance Outputable TypedID where
    ppr (TypedID x t) = ppr x <> ":" <> ppr t

instance Typeable TypedID where
    typeOf (TypedID _ t) = t
