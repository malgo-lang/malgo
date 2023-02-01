{-# LANGUAGE DeriveAnyClass #-}

module Koriel.Core.Type (Tag (..), Con (..), Type (..), HasType (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Binary.Instances.UnorderedContainers ()
import Data.Data (Data)
import Data.HashMap.Strict qualified as HashMap
import Koriel.Id
import Koriel.Prelude
import Koriel.Pretty

{-
Constructors  C ::= <tag n>
-}
data Tag
  = Data Text
  | Tuple
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)
  deriving anyclass (Hashable, Binary, ToJSON, FromJSON)

instance Pretty Tag where
  pPrint (Data name) = pPrint name
  pPrint Tuple = "Tuple#"

data Con = Con Tag [Type]
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)
  deriving anyclass (Hashable, Binary, ToJSON, FromJSON)

instance Pretty Con where
  pPrint (Con tag xs) = parens $ sep $ pPrint tag : map pPrint xs

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
  | RecordT (HashMap Text Type)
  | AnyT
  | VoidT
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)
  deriving anyclass (Hashable, Binary, ToJSON, FromJSON)

instance Pretty Type where
  pPrint (a :-> b) = parens $ sep ["->", brackets (sep $ map pPrint a), pPrint b]
  pPrint Int32T = "Int32#"
  pPrint Int64T = "Int64#"
  pPrint FloatT = "Float#"
  pPrint DoubleT = "Double#"
  pPrint CharT = "Char#"
  pPrint StringT = "String#"
  pPrint BoolT = "Bool#"
  pPrint (SumT cs) = parens $ sep ("sum" : map pPrint cs)
  pPrint (PtrT t) = parens $ "Ptr#" <+> pPrint t
  pPrint (RecordT fs) = parens $ "Record#" <+> sep (map (\(k, v) -> parens $ sep [pPrint k, pPrint v]) $ HashMap.toList fs)
  pPrint AnyT = "Any#"
  pPrint VoidT = "Void#"

class HasType a where
  typeOf :: a -> Type

instance HasType Type where
  typeOf x = x

instance HasType a => HasType (Id a) where
  typeOf x = typeOf $ x.meta