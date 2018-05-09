{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Language.Malgo.Type where

import           Language.Malgo.Prelude
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

instance Outputable Type where
    ppr "Unit" = "{}"
    ppr (NameTy n) = ppr n
    ppr (FunTy [param] ret) =
      ppr param <+> "->" <+> ppr ret
    ppr (FunTy params ret) =
      parens (ppr params) <+>
      "->" <+> ppr ret
    ppr (TupleTy xs) = braces $ ppr xs
    ppr (ClsTy [param] ret) =
      braces (ppr param <+> "->" <+> ppr ret)
    ppr (ClsTy params ret) =
        braces (parens (ppr params) <+>
                "->" <+> ppr ret)

instance IsString Type where
    fromString name = NameTy $ fromString name

class Typeable a where
    typeOf :: a -> Type
