{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.TypeRep.Type where

import Language.Malgo.Prelude
import Language.Malgo.Pretty
import Text.PrettyPrint.HughesPJClass (braces, parens, punctuate, sep)

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

class HasType a where
  typeOf :: a -> Type

instance HasType Type where
  typeOf = id

comparable :: Type -> Bool
comparable (TyApp IntC []) = True
comparable (TyApp FloatC []) = True
comparable (TyApp BoolC []) = True
comparable (TyApp CharC []) = True
comparable _ = False

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
