{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
module Language.Malgo.TypeRep.CType where

import           Language.Malgo.Id
import           Language.Malgo.Prelude
-- import qualified Language.Malgo.TypeRep.Type as T

data CType = CType :-> CType
    | IntT
    | FloatT
    | CharT
    | StringT
    | PackT [Con]
    | ArrayT CType
    | AnyT
    deriving stock (Eq, Show, Ord)

{-
Constructors  C ::= <tag n>
-}
type Tag = Text

data Con = Con Tag [CType]
    deriving stock (Eq, Show, Ord)

class HasCType a where
  cTypeOf :: a -> CType

instance HasCType CType where
  cTypeOf x = x

instance HasCType a => HasCType (Id a) where
  cTypeOf x = cTypeOf $ x ^. idMeta
