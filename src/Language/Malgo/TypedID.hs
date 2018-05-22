{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.TypedID
  ( TypedID
  ) where

import           Language.Malgo.ID
import           Language.Malgo.Prelude
import           Language.Malgo.Type

-- data TypedID = TypedID {_id :: ID, _type :: Type}
--     deriving (Show, Ord, Read)

type TypedID = ID Type
-- instance Eq TypedID where
--     (TypedID x _) == (TypedID y _) = x == y

-- instance Outputable TypedID where
--     ppr (TypedID x t) = ppr x <> ":" <> ppr t

-- instance Typeable TypedID where
--     typeOf (TypedID _ t) = t
