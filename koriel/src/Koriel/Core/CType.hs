{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Koriel.Core.CType where

import Data.Set (fromList)
import Koriel.Id
import Koriel.Prelude
import Koriel.Pretty

-- TODO: クロージャを表す型を追加
-- ClosureT [CType] CType
-- あるいはknown関数を表す型
-- FuncT [CType] CType
data CType
  = [CType] :-> CType
  | Int32T
  | Int64T
  | FloatT
  | DoubleT
  | CharT
  | StringT
  | DataT String [CType]
  | SumT (Set Con)
  | ArrayT CType
  | AnyT
  deriving stock (Eq, Show, Ord)

instance Pretty CType where
  pPrint (a :-> b) = parens (sep $ map pPrint a) <+> "->" <+> pPrint b
  pPrint Int32T = "Int32#"
  pPrint Int64T = "Int64#"
  pPrint FloatT = "Float#"
  pPrint DoubleT = "Double#"
  pPrint CharT = "Char#"
  pPrint StringT = "String#"
  pPrint (DataT n ts) = parens $ pPrint n <+> sep (map pPrint ts)
  pPrint (SumT cs) = braces $ sep (map pPrint $ toList cs)
  pPrint (ArrayT t) = brackets $ pPrint t
  pPrint AnyT = "*"

{-
Constructors  C ::= <tag n>
-}
type Tag = String

data Con = Con Tag [CType]
  deriving stock (Eq, Show, Ord)

instance Pretty Con where
  pPrint (Con tag xs) = "<" <> text tag <+> sep (punctuate "," (map pPrint xs)) <> ">"

class HasCType a where
  cTypeOf :: HasCallStack => a -> CType

instance HasCType CType where
  cTypeOf x = x

instance HasCType a => HasCType (Id a) where
  cTypeOf x = cTypeOf $ x ^. idMeta

tyVar :: Traversal CType CType CType CType
tyVar = traversalVL $ \f -> \case
  ps :-> r -> (:->) <$> traverseOf (traversed % tyVar) f ps <*> traverseOf tyVar f r
  DataT n ts -> DataT n <$> traverseOf (traversed % tyVar) f ts
  SumT cs -> SumT . fromList <$> traverseOf (traversed % tyVar') f (toList cs)
  ArrayT t -> ArrayT <$> traverseOf tyVar f t
  AnyT -> f AnyT
  t -> pure t

tyVar' :: Traversal Con Con CType CType
tyVar' = traversalVL $ \f (Con tag ts) -> Con tag <$> traverseOf (traversed % tyVar) f ts
