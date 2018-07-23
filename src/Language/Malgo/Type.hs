{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Language.Malgo.Type where

import           RIO                            hiding (Typeable)
import Data.Text.Prettyprint.Doc

-- | Malgoの組み込みデータ型
data Type
  = NameTy Text
  | FunTy { _params :: [Type]
          , _ret    :: Type }
  | TupleTy [Type]
  deriving (Eq, Show, Ord, Read)

instance Pretty Type where
  pretty "Unit" = "{}"
  pretty (NameTy n) = pretty n
  pretty (FunTy [param] ret) =
    pretty param <+> "->" <+> pretty ret
  pretty (FunTy params ret) =
    parens (sep $ punctuate "," (map pretty params)) <+>
    "->" <+> pretty ret
  pretty (TupleTy xs) =
    braces $ sep $ punctuate "," $ map pretty xs

instance IsString Type where
  fromString name = NameTy $ fromString name

class Typeable a where
  typeOf :: a -> Type

instance Typeable Type where
  typeOf = id
