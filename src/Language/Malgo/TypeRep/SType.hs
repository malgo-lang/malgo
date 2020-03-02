{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.Malgo.TypeRep.SType where

import           Language.Malgo.Prelude
import           Language.Malgo.Pretty

import           Text.PrettyPrint.HughesPJClass ( parens
                                                , sep
                                                , punctuate
                                                , braces
                                                , brackets
                                                )

data SType a = TyVar a
             | TyInt
             | TyFloat
             | TyBool
             | TyChar
             | TyString
             | TyFun [SType a] (SType a)
             | TyTuple [SType a]
             | TyArray (SType a)
             | TyForall [a] (SType a)
  deriving stock (Eq, Show, Functor, Foldable)

instance Pretty a => Pretty (SType a) where
  pPrint (TyVar x)       = pPrint x
  pPrint TyInt           = "Int"
  pPrint TyFloat         = "Float"
  pPrint TyBool          = "Bool"
  pPrint TyChar          = "Char"
  pPrint TyString        = "String"
  pPrint (TyFun ps r   ) = parens (sep $ punctuate "," $ map pPrint ps) <+> "->" <+> pPrint r
  pPrint (TyTuple ts   ) = braces (sep $ punctuate "," $ map pPrint ts)
  pPrint (TyArray t    ) = brackets $ pPrint t
  pPrint (TyForall ts t) = "forall" <+> sep (map pPrint ts) <> "." <+> pPrint t
