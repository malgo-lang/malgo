{-# LANGUAGE TemplateHaskell #-}

module Koriel.Core.Type where

import Data.Binary (Binary)
import Koriel.Id
import Koriel.Prelude hiding ((.=))
import Koriel.Pretty

{-
Constructors  C ::= <tag n>
-}
data Tag
  = Data String
  | Tuple
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)

instance Binary Tag

instance Pretty Tag where
  pPrint (Data name) = text name
  pPrint Tuple = "#tuple"

data Con = Con Tag [Type]
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)

instance Binary Con

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

instance Binary Type

instance Pretty Type where
  pPrint (a :-> b) = brackets (sep $ punctuate "," $ map pPrint a) <+> "->" <+> pPrint b
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

tyVar :: Traversal' Type Type
tyVar f = \case
  ps :-> r -> (:->) <$> traverseOf (traversed . tyVar) f ps <*> traverseOf tyVar f r
  SumT cs -> SumT <$> traverseOf (traversed . tyVar') f (toList cs)
  AnyT -> f AnyT
  t -> pure t

tyVar' :: Traversal' Con Type
tyVar' f (Con tag ts) = Con tag <$> traverseOf (traversed . tyVar) f ts

makePrisms ''Type
