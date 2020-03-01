{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Language.Malgo.TypeRep.LType where

import           Language.Malgo.ID
import           Language.Malgo.Prelude
import           Language.Malgo.Pretty

import           Relude.Unsafe
import           Text.PrettyPrint.HughesPJClass ( braces
                                                , sep
                                                , punctuate
                                                , parens
                                                , text
                                                )

data LType = Ptr LType
           | Bit
           | I32
           | I64
           | U8
           | U32
           | U64
           | F64
           | SizeT
           | Struct [LType]
           | Function LType [LType]
           | Void
  deriving stock (Eq, Ord, Show)

instance Pretty LType where
  pPrint (Ptr    t ) = pPrint t <> "*"
  pPrint (Struct xs) = braces (sep $ punctuate "," $ map pPrint xs)
  pPrint (Function ret params) =
    parens (sep $ punctuate "," $ map pPrint params) <> "->" <> pPrint ret
  pPrint t = text $ toString $ pShow t

class HasLType a where
  ltypeOf :: a -> LType

instance HasLType LType where
  ltypeOf x = x

instance HasLType a => HasLType (ID a) where
  ltypeOf ID { idMeta } = ltypeOf idMeta

accessType :: LType -> [Int] -> LType
accessType t           []       = t
accessType (Ptr    t ) (_ : xs) = accessType t xs
accessType (Struct ts) (i : xs) = accessType (ts !! i) xs
accessType t           _        = error $ toText $ pShow t <> " is not accessable"
