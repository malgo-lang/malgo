{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Language.Malgo.Type where

import           RIO                            hiding (Typeable)
import qualified RIO.Text                       as Text
import           Text.PrettyPrint
import           Text.PrettyPrint.HughesPJClass

-- | Malgoの組み込みデータ型
data Type
  = NameTy Text
  | FunTy { _params :: [Type]
          , _ret    :: Type }
  | TupleTy [Type]
  deriving (Eq, Show, Ord, Read)

instance Pretty Type where
  pPrint "Unit" = "{}"
  pPrint (NameTy n) = text $ Text.unpack n
  pPrint (FunTy [param] ret) =
    pPrint param <+> "->" <+> pPrint ret
  pPrint (FunTy params ret) =
    parens (sep $ punctuate "," (map pPrint params)) <+>
    "->" <+> pPrint ret
  pPrint (TupleTy xs) =
    braces $ sep $ punctuate "," $ map pPrint xs

instance IsString Type where
  fromString name = NameTy $ fromString name

class Typeable a where
  typeOf :: a -> Type

instance Typeable Type where
  typeOf = id
