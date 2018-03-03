{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData        #-}
module Language.Malgo.Type where

import           Language.Malgo.Prelude
import           Language.Malgo.Utils
import           Text.PrettyPrint

-- | Malgoの組み込みデータ型
data Type
    = NameTy Name
    | FunTy { _params :: [Type]
            , _ret    :: Type }
    | TupleTy [Type]
    | ClsTy { _params :: [Type]
            , _ret    :: Type }
    deriving (Eq, Show, Ord, Read)

instance PrettyPrint Type where
    pretty "Unit" = "{}"
    pretty (NameTy n) = pretty n
    pretty (FunTy [param] ret) =
      pretty param <+> "->" <+> pretty ret
    pretty (FunTy params ret) =
      parens (cat (punctuate "," (map pretty params))) <+>
      "->" <+> pretty ret
    pretty (TupleTy xs) = braces $ sep (punctuate "," (map pretty xs))
    pretty (ClsTy [param] ret) =
      braces (pretty param <+> "->" <+> pretty ret)
    pretty (ClsTy params ret) =
        braces
            (parens (cat (punctuate "," (map pretty params))) <+>
             "->" <+> pretty ret)

instance IsString Type where
    fromString name = NameTy $ fromString name

class Typeable a where
    typeOf :: a -> Type
