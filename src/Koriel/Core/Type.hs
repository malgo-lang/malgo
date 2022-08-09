module Koriel.Core.Type where

import Control.Lens (Prism', prism, (^.))
import Data.Aeson
import Data.Binary (Binary)
import Data.Data (Data)
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

instance Binary Tag

instance ToJSON Tag

instance FromJSON Tag

instance Pretty Tag where
  pPrint (Data name) = pPrint name
  pPrint Tuple = "#tuple"

data Con = Con Tag [Type]
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)

instance Binary Con

instance ToJSON Con

instance FromJSON Con

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
  | AnyT
  | VoidT
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)

_SumT :: Prism' Type [Con]
_SumT = prism SumT \case
  SumT cs -> Right cs
  t -> Left t

instance Binary Type

instance ToJSON Type

instance FromJSON Type

instance Pretty Type where
  pPrint (a :-> b) = sep [brackets (sep $ punctuate "," $ map pPrint a), "->", pPrint b]
  pPrint Int32T = "Int32#"
  pPrint Int64T = "Int64#"
  pPrint FloatT = "Float#"
  pPrint DoubleT = "Double#"
  pPrint CharT = "Char#"
  pPrint StringT = "String#"
  pPrint BoolT = "Bool#"
  pPrint (SumT cs) = parens $ sep ("sum" : map pPrint cs)
  pPrint (PtrT t) = parens $ "Ptr#" <+> pPrint t
  pPrint AnyT = "Any#"
  pPrint VoidT = "Void#"

class HasType a where
  typeOf :: a -> Type

instance HasType Type where
  typeOf x = x

instance HasType a => HasType (Id a) where
  typeOf x = typeOf $ x ^. idMeta