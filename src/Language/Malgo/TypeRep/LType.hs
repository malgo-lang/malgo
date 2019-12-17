{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms   #-}
module Language.Malgo.TypeRep.LType where

import           Language.Malgo.ID
import           Language.Malgo.Pretty
import           Relude                hiding (Type)

data LType = Ptr LType
           | Bit
           | I32
           | I64
           | U8
           | U32
           | U64
           | Double
           | Struct [LType]
           | Function LType [LType]
           | Void
  deriving (Eq, Show, Read, Generic, PrettyVal)

pattern Boxed :: LType
pattern Boxed = Ptr Void

class HasLType a where
  ltypeOf :: a -> LType

instance HasLType LType where
  ltypeOf x = x

instance HasLType a => HasLType (ID a) where
  ltypeOf x = ltypeOf $ _idMeta x
