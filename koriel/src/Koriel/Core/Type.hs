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
  pretty (Data name) = pretty name
  pretty Tuple = "#tuple"

data Con = Con Tag [Type]
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)

instance Binary Con

instance Pretty Con where
  pretty (Con tag xs) = parens $ hang 1 $ sep $ pretty tag : map pretty xs

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
  pretty (a :-> b) = parens $ hang 1 $ sep ["->", parens $ sep $ map pretty a, pretty b]
  pretty Int32T = "Int32#"
  pretty Int64T = "Int64#"
  pretty FloatT = "Float#"
  pretty DoubleT = "Double#"
  pretty CharT = "Char#"
  pretty StringT = "String#"
  pretty BoolT = "Bool#"
  pretty (SumT cs) = parens $ hang 1 $ sep ("sum" : map pretty cs)
  pretty (PtrT t) = parens $ "Ptr#" <+> pretty t
  pretty AnyT = "Any#"
  pretty VoidT = "Void#"

makePrisms ''Type

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
