{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}
module Language.Malgo.TypeRep.CType where

import           Language.Malgo.Prelude
import Language.Malgo.Id

data CType = CType :-> CType
    | IntT
    | FloatT
    | CharT
    | StringT
    | PackT Text [CType]
    | AnyT
    deriving stock (Eq, Show, Ord)

class HasCType a where
    cTypeOf :: a -> CType

instance HasCType CType where
    cTypeOf x = x

instance HasCType a => HasCType (Id a) where
    cTypeOf x = cTypeOf $ x ^. idMeta