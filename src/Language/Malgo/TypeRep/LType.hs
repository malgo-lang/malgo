{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.TypeRep.LType where

import           Language.Malgo.ID
import           Language.Malgo.Pretty
import           Relude                hiding (Type)
import           Relude.Unsafe         ((!!))

data LType = Ptr LType
           | Bit
           | I32
           | I64
           | U8
           | U32
           | U64
           | F64
           | Struct [LType]
           | Function LType [LType]
           | Void
  deriving (Eq, Ord, Show, Read, Generic)

instance Pretty LType where
  pPrint (Ptr t) = "ptr" <> parens (pPrint t)
  pPrint (Struct xs) = braces (sep $ punctuate "," $ map pPrint xs)
  pPrint (Function ret params) = parens (sep $ punctuate "," $ map pPrint params) <> "->" <> pPrint ret
  pPrint t = text $ toString $ pShow t

class HasLType a where
  ltypeOf :: a -> LType

instance HasLType LType where
  ltypeOf x = x

instance HasLType a => HasLType (ID a) where
  ltypeOf ID{ idMeta } = ltypeOf idMeta

accessType :: LType -> [Int] -> LType
accessType t []               = t
accessType (Ptr t) (_:xs)     = accessType t xs
accessType (Struct ts) (i:xs) = accessType (ts !! i) xs
accessType t _ = error $ toText $ pShow t <> " is not accessable"
