{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Malgo.Core.Type (Tag (..), Con (..), Type (..), HasType (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Map.Strict qualified as Map
import Data.Store.TH
import Malgo.Id
import Malgo.Prelude

{-
Constructors  C ::= <tag n>
-}
data Tag
  = Data Text
  | Tuple
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)
  deriving anyclass (Hashable, ToJSON, FromJSON)

makeStore ''Tag

instance Pretty Tag where
  pretty (Data name) = pretty name
  pretty Tuple = "Tuple#"

data Con = Con Tag [Type]
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)
  deriving anyclass (Hashable, ToJSON, FromJSON)

instance Pretty Con where
  pretty (Con tag xs) = parens $ sep $ pretty tag : map pretty xs

-- TODO: クロージャを表す型を追加
-- ClosureT [Type] Type
-- あるいはknown関数を表す型
-- FuncT [Type] Type
data Type
  = [Type] :-> Type
  | Int32T
  | Int64T
  | FloatT
  | DoubleT
  | CharT
  | StringT
  | BoolT
  | SumT [Con]
  | PtrT Type
  | RecordT (Map Text Type)
  | AnyT
  | VoidT
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)
  deriving anyclass (Hashable, ToJSON, FromJSON)

instance Pretty Type where
  pretty (a :-> b) = parens $ sep ["->", brackets (sep $ map pretty a), pretty b]
  pretty Int32T = "Int32#"
  pretty Int64T = "Int64#"
  pretty FloatT = "Float#"
  pretty DoubleT = "Double#"
  pretty CharT = "Char#"
  pretty StringT = "String#"
  pretty BoolT = "Bool#"
  pretty (SumT cs) = parens $ sep ("sum" : map pretty cs)
  pretty (PtrT t) = parens $ "Ptr#" <+> pretty t
  pretty (RecordT fs) = parens $ "Record#" <+> sep (map (\(k, v) -> parens $ sep [pretty k, pretty v]) $ Map.toList fs)
  pretty AnyT = "Any#"
  pretty VoidT = "Void#"

$( liftA2
     (<>)
     (makeStore ''Con)
     (makeStore ''Type)
 )

class HasType a where
  typeOf :: a -> Type

instance HasType Type where
  typeOf x = x

instance (HasType a) => HasType (Meta a) where
  typeOf x = typeOf x.meta
