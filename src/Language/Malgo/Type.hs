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
    pretty "Unit" = text "{}"
    pretty (NameTy n) = pretty n
    pretty (FunTy param ret) =
        parens
            (cat (punctuate (text ",") (map pretty param)) <+>
             text "->" <+> pretty ret)
    pretty (TupleTy xs) = braces $ sep (punctuate (text ",") (map pretty xs))
    pretty (ClsTy param ret) =
        braces
            (parens
                 (cat (punctuate (text ",") (map pretty param)) <+>
                  text "->" <+> pretty ret))

instance IsString Type where
    fromString name = NameTy $ fromString name

class Typeable a where
    typeOf :: a -> Type
