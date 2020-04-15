{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
module Language.Malgo.TypeRep.CType where

import           Data.Text                   (pack)
import           Language.Malgo.Id
import           Language.Malgo.Prelude
import qualified Language.Malgo.TypeRep.Type as T

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

instance HasCType T.Type where
  cTypeOf (T.TyApp T.IntC []) = PackT [Con "Int" [IntT]]
  cTypeOf (T.TyApp T.FloatC []) = PackT [Con "Float" [FloatT]]
  cTypeOf (T.TyApp T.BoolC []) = PackT [Con "True" [], Con "False" []]
  cTypeOf (T.TyApp T.CharC []) = PackT [Con "Char" [CharT]]
  cTypeOf (T.TyApp T.StringC []) = PackT [Con "String" [StringT]]
  cTypeOf (T.TyApp T.TupleC xs) = PackT [Con ("Tuple" <> pack (show $ length xs)) (map cTypeOf xs)]
  cTypeOf (T.TyApp T.ArrayC [x]) = ArrayT (cTypeOf x)
  cTypeOf (T.TyApp _ _) = bug Unreachable
  cTypeOf (T.TyMeta _) = AnyT
  cTypeOf (ps T.:-> r) = PackT [Con ("Tuple" <> pack (show $ length ps)) (map cTypeOf ps)] :-> cTypeOf r
  cTypeOf T.Kind = AnyT
