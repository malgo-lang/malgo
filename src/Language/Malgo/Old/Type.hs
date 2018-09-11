{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Language.Malgo.Old.Type where

import           Data.Outputable
import           Language.Malgo.Old.Pretty
import           Universum             hiding (Type)

-- | Malgoの組み込みデータ型
data Type
  = NameTy Text
  | FunTy { _params :: [Type]
          , _ret    :: Type }
  | TupleTy [Type]
  | ArrayTy Type
  deriving (Eq, Show, Ord, Read, Generic, Outputable)

instance Pretty Type where
  pPrint "Unit" = "{}"
  pPrint (NameTy n) = pPrint n
  pPrint (FunTy [param] ret) =
    pPrint param <+> "->" <+> pPrint ret
  pPrint (FunTy params ret) =
    parens (sep $ punctuate "," (map pPrint params)) <+>
    "->" <+> pPrint ret
  pPrint (TupleTy xs) =
    braces $ sep $ punctuate "," $ map pPrint xs
  pPrint (ArrayTy t) =
    brackets $ pPrint t

instance IsString Type where
  fromString name = NameTy $ fromString name

class HasType a where
  typeOf :: a -> Type

instance HasType Type where
  typeOf = id
