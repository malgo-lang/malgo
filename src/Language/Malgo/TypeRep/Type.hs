{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.TypeRep.Type where

import           Language.Malgo.Pretty
import           Language.Malgo.Prelude

type TyVar = Int

-- | Malgoの組み込みデータ型
data Type = TyApp TyCon [Type]
          | TyMeta TyVar
          | TyForall [TyVar] Type
  deriving (Eq, Show, Ord, Read, Generic)

data TyCon = FunC | IntC | FloatC | BoolC | CharC | StringC | TupleC | ArrayC
  deriving (Eq, Show, Ord, Read, Generic)

instance Pretty Type where
  pPrint (TyApp FunC (ret : params)) =
    parens (sep $ punctuate "," $ map pPrint params) <+> "->" <+> pPrint ret
  pPrint (TyApp TupleC ts) = braces $ sep $ punctuate "," $ map pPrint ts
  pPrint (TyApp c ts) =
    pPrint c <> parens (sep $ punctuate "," $ map pPrint ts)
  pPrint (TyMeta v) = pPrint v
  pPrint (TyForall ts t) = "forall" <+> sep (map pPrint ts) <> "." <+> pPrint t

instance Pretty TyCon where
  pPrint FunC    = "Fun"
  pPrint IntC    = "Int"
  pPrint FloatC  = "Float"
  pPrint BoolC   = "Bool"
  pPrint CharC   = "Char"
  pPrint StringC = "String"
  pPrint TupleC  = "Tuple"
  pPrint ArrayC  = "Array"

class HasType a where
  typeOf :: a -> Type

instance HasType Type where
  typeOf = id

comparable :: Type -> Bool
comparable (TyApp IntC   []) = True
comparable (TyApp FloatC []) = True
comparable (TyApp BoolC  []) = True
comparable (TyApp CharC  []) = True
comparable _                 = False
