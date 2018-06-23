{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.Malgo.Type where

import           Language.Malgo.Prelude

-- | Malgoの組み込みデータ型
data Type
  = NameTy Name
  | FunTy { _params :: [Type]
          , _ret    :: Type }
  | TupleTy [Type]
  | ClsTy { _params :: [Type]
          , _ret    :: Type }
  deriving (Eq, Show, Ord, Read)

instance Pretty Type where
  pretty "Unit" = "{}"
  pretty (NameTy n) = pretty n
  pretty (FunTy [param] ret) =
    pretty param <+> "->" <+> pretty ret
  pretty (FunTy params ret) =
    tupled (map pretty params) <+>
    "->" <+> pretty ret
  pretty (TupleTy xs) =
    braces $ vsep $ punctuate "," $ map pretty xs
  pretty (ClsTy [param] ret) =
    braces (pretty param <+> "->" <+> pretty ret)
  pretty (ClsTy params ret) =
      braces (tupled (map pretty params) <+>
              "->" <+> pretty ret)

instance IsString Type where
  fromString name = NameTy $ fromString name

class Typeable a where
  typeOf :: a -> Type

instance Typeable Type where
  typeOf = identity
