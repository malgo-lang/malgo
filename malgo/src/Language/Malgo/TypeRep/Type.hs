{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.TypeRep.Type where

import Koriel.Core.Type hiding (HasType (..), Type, (:->))
import qualified Koriel.Core.Type as C
import Koriel.Id
import Koriel.Prelude
import Koriel.Pretty

type TyVar = Int

data Scheme = Forall [TyVar] Type
  deriving stock (Eq, Show, Ord)

-- | Malgoの組み込みデータ型
data Type
  = TyApp TyCon [Type]
  | TyMeta TyVar
  | [Type] :-> Type
  deriving stock (Eq, Show, Ord)

infixl 6 :->

data TyCon
  = IntC
  | FloatC
  | BoolC
  | CharC
  | StringC
  | TupleC
  | ArrayC
  deriving stock (Eq, Show, Ord)

instance Pretty Scheme where
  pPrint (Forall ts t) = "forall" <+> sep (map pPrint ts) <> "." <+> pPrint t

instance Pretty Type where
  pPrint (TyApp TupleC ts) = braces $ sep $ punctuate "," $ map pPrint ts
  pPrint (TyApp c ts) = pPrint c <> parens (sep $ punctuate "," $ map pPrint ts)
  pPrint (TyMeta v) = pPrint v
  pPrint (params :-> ret) = parens (sep $ punctuate "," $ map pPrint params) <> "->" <> pPrint ret

instance Pretty TyCon where
  pPrint IntC = "Int"
  pPrint FloatC = "Float"
  pPrint BoolC = "Bool"
  pPrint CharC = "Char"
  pPrint StringC = "String"
  pPrint TupleC = "Tuple"
  pPrint ArrayC = "Array"

instance C.HasType Type where
  typeOf (TyApp IntC []) = SumT [Con "Int" [Int64T]]
  typeOf (TyApp FloatC []) = SumT [Con "Float" [DoubleT]]
  typeOf (TyApp BoolC []) = SumT [Con "True" [], Con "False" []]
  typeOf (TyApp CharC []) = SumT [Con "Char" [CharT]]
  typeOf (TyApp StringC []) = SumT [Con "String" [StringT]]
  typeOf (TyApp TupleC xs) = SumT [Con ("Tuple" <> show (length xs)) (map C.typeOf xs)]
  typeOf (TyApp ArrayC [x]) = ArrayT (C.typeOf x)
  typeOf (TyApp _ _) = bug Unreachable
  typeOf TyMeta {} = AnyT
  typeOf (ps :-> r) = map C.typeOf ps C.:-> C.typeOf r

class HasType a where
  typeOf :: a -> Type

instance HasType Type where
  typeOf = id

instance HasType a => HasType (Id a) where
  typeOf n = n ^. idMeta % to typeOf

removeExplictForall :: Scheme -> Type
removeExplictForall (Forall _ t) = t

intTy :: Type
intTy = TyApp IntC []

floatTy :: Type
floatTy = TyApp FloatC []

boolTy :: Type
boolTy = TyApp BoolC []

charTy :: Type
charTy = TyApp CharC []

stringTy :: Type
stringTy = TyApp StringC []

tupleTy :: [Type] -> Type
tupleTy = TyApp TupleC

arrayTy :: Type -> Type
arrayTy x = TyApp ArrayC [x]
