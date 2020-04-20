{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
module Language.Malgo.TypeRep.CType where

import           Data.Text                   (pack, unpack)
import           Language.Malgo.Id
import           Language.Malgo.Prelude
import           Language.Malgo.Pretty
import qualified Language.Malgo.TypeRep.Type as T
import           Text.PrettyPrint.HughesPJ   (braces, brackets, parens, sep,
                                              text)

data CType = CType :-> CType
    | IntT
    | FloatT
    | CharT
    | StringT
    | PackT [Con]
    | ArrayT CType
    | AnyT
    deriving stock (Eq, Show, Ord)

instance Pretty CType where
  pPrint (a@(_ :-> _) :-> b) = parens (pPrint a) <+> "->" <+> pPrint b
  pPrint (a :-> b)           = pPrint a <+> "->" <+> pPrint b
  pPrint IntT                = "Int#"
  pPrint FloatT              = "Float#"
  pPrint CharT               = "Char#"
  pPrint StringT             = "String#"
  pPrint (PackT ts)          = braces $ sep (map pPrint ts)
  pPrint (ArrayT t)          = brackets $ pPrint t
  pPrint AnyT                = "Any"

{-
Constructors  C ::= <tag n>
-}
type Tag = Text

data Con = Con Tag [CType]
    deriving stock (Eq, Show, Ord)

instance Pretty Con where
  pPrint (Con tag xs) = "<" <> text (unpack tag) <+> sep (map pPrint xs) <> ">"

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
