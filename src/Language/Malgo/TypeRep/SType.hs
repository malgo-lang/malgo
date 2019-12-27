{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric     #-}
module Language.Malgo.TypeRep.SType where

import           Language.Malgo.Prelude
import           Language.Malgo.Pretty

data SType = TyVar Text
           | TyInt
           | TyFloat
           | TyBool
           | TyChar
           | TyString
           | TyFun [SType] SType
           | TyTuple [SType]
           | TyArray SType
           | TyForall [Text] SType
  deriving (Eq, Show, Read, Generic)

instance Pretty SType where
  pPrint (TyVar x) = text $ toString x
  pPrint TyInt     = "Int"
  pPrint TyFloat   = "Float"
  pPrint TyBool    = "Bool"
  pPrint TyChar    = "Char"
  pPrint TyString  = "String"
  pPrint (TyFun ps r) =
    parens (sep $ punctuate "," $ map pPrint ps) <+> "->" <+> pPrint r
  pPrint (TyTuple ts) = braces (sep $ punctuate "," $ map pPrint ts)
  pPrint (TyArray t ) = brackets $ pPrint t
  pPrint (TyForall ts t) =
    "forall" <+> sep (map (text . toString) ts) <> "." <+> pPrint t
