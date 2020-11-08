{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Koriel.Core.Type where

import Data.Set (fromList)
import Koriel.Id
import Koriel.Prelude
import Koriel.Pretty

{-
Constructors  C ::= <tag n>
-}
type Tag = String

data Con = Con Tag [Type]
  deriving stock (Eq, Show, Ord)

instance Pretty Con where
  pPrint (Con tag xs) = "<" <> text tag <+> sep (punctuate "," (map pPrint xs)) <> ">"

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
  | DataT String [Type]
  | SumT (Set Con)
  | ArrayT Type
  | AnyT
  deriving stock (Eq, Show, Ord)

_SumT :: Prism' Type (Set Con)
_SumT = prism' SumT $ \case
  SumT sc -> Just sc
  _ -> Nothing

instance Pretty Type where
  pPrint (a :-> b) = brackets (sep $ punctuate "," $ map pPrint a) <+> "->" <+> pPrint b
  pPrint Int32T = "Int32#"
  pPrint Int64T = "Int64#"
  pPrint FloatT = "Float#"
  pPrint DoubleT = "Double#"
  pPrint CharT = "Char#"
  pPrint StringT = "String#"
  pPrint BoolT = "Bool#"
  pPrint (DataT n ts) = parens $ pPrint n <+> sep (map pPrint ts)
  pPrint (SumT cs) = braces $ sep (map pPrint $ toList cs)
  pPrint (ArrayT t) = brackets $ pPrint t
  pPrint AnyT = "*"

class HasType a where
  typeOf :: HasCallStack => a -> Type

instance HasType Type where
  typeOf x = x

instance HasType a => HasType (Id a) where
  typeOf x = typeOf $ x ^. idMeta

tyVar :: Traversal' Type Type
tyVar f = \case
  ps :-> r -> (:->) <$> traverseOf (traversed . tyVar) f ps <*> traverseOf tyVar f r
  DataT n ts -> DataT n <$> traverseOf (traversed . tyVar) f ts
  SumT cs -> SumT . fromList <$> traverseOf (traversed . tyVar') f (toList cs)
  ArrayT t -> ArrayT <$> traverseOf tyVar f t
  AnyT -> f AnyT
  t -> pure t

tyVar' :: Traversal' Con Type
tyVar' f (Con tag ts) = Con tag <$> traverseOf (traversed . tyVar) f ts
