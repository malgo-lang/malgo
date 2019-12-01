{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE StrictData        #-}
module Language.Malgo.TypeRep.Type where

import           Language.Malgo.Pretty
import           Relude                hiding (Type)

type TyVar = Int

-- | Malgoの組み込みデータ型
data Type = TyApp TyCon [Type]
          | TyMeta TyVar
  deriving (Eq, Show, Ord, Read, Generic, PrettyVal)

data TyCon = FunC Type | IntC | FloatC | BoolC | CharC | StringC | TupleC | ArrayC
  deriving (Eq, Show, Ord, Read, Generic, PrettyVal)

instance Pretty Type where
  pPrint (TyApp (FunC ret) params) = parens (sep $ punctuate "," $ map pPrint params) <+> "->" <+> pPrint ret
  pPrint (TyApp TupleC ts) = braces $ sep $ punctuate "," $ map pPrint ts
  pPrint (TyApp c ts) = pPrint c <> parens (sep $ punctuate "," $ map pPrint ts)
  pPrint (TyMeta v) = pPrint v

instance Pretty TyCon where
  pPrint (FunC t) = parens $ "_ ->" <+> pPrint t
  pPrint IntC     = "Int"
  pPrint FloatC   = "Float"
  pPrint BoolC    = "Bool"
  pPrint CharC    = "Char"
  pPrint StringC  = "String"
  pPrint TupleC   = "Tuple"
  pPrint ArrayC   = "Array"

class HasType a where
  typeOf :: a -> Type

instance HasType Type where
  typeOf = id

pattern TyInt, TyFloat, TyBool, TyChar, TyString :: Type
pattern TyTuple :: [Type] -> Type
pattern TyArray :: Type -> Type
pattern TyFun :: [Type] -> Type -> Type
pattern TyInt = TyApp IntC []
pattern TyFloat = TyApp FloatC []
pattern TyBool = TyApp BoolC []
pattern TyChar = TyApp CharC []
pattern TyString = TyApp StringC []
pattern TyTuple xs = TyApp TupleC xs
pattern TyArray x = TyApp ArrayC [x]
pattern TyFun ps r = TyApp (FunC r) ps

comparable :: Type -> Bool
comparable TyInt   = True
comparable TyFloat = True
comparable TyBool  = True
comparable TyChar  = True
comparable _       = False
