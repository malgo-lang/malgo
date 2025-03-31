{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Malgo.Core.Type (Tag (..), Con (..), Type (..), HasType (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Map.Strict qualified as Map
import Data.SCargot.Repr.Basic qualified as S
import Data.Store.TH
import Malgo.Id
import Malgo.Prelude
import Malgo.SExpr qualified as S
import Prettyprinter (brackets, parens, sep, (<+>))

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

instance S.ToSExpr Tag where
  toSExpr (Data name) = S.A $ S.Symbol name
  toSExpr Tuple = S.A $ S.Symbol "Tuple#"

data Con = Con Tag [Type]
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)
  deriving anyclass (Hashable, ToJSON, FromJSON)

instance Pretty Con where
  pretty (Con tag xs) = parens $ sep $ pretty tag : map pretty xs

instance S.ToSExpr Con where
  toSExpr (Con tag xs) = S.L $ S.A (S.Symbol "con") : S.toSExpr tag : map S.toSExpr xs

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

instance S.ToSExpr Type where
  toSExpr (a :-> b) = S.L [S.A (S.Symbol "->"), S.L (map S.toSExpr a), S.toSExpr b]
  toSExpr Int32T = S.A (S.Symbol "Int32#")
  toSExpr Int64T = S.A (S.Symbol "Int64#")
  toSExpr FloatT = S.A (S.Symbol "Float#")
  toSExpr DoubleT = S.A (S.Symbol "Double#")
  toSExpr CharT = S.A (S.Symbol "Char#")
  toSExpr StringT = S.A (S.Symbol "String#")
  toSExpr BoolT = S.A (S.Symbol "Bool#")
  toSExpr (SumT cs) = S.L (S.A (S.Symbol "sum") : map S.toSExpr cs)
  toSExpr (PtrT t) = S.L [S.A (S.Symbol "Ptr#"), S.toSExpr t]
  toSExpr (RecordT fs) = S.L (S.A (S.Symbol "Record#") : map (\(k, v) -> S.L [S.A $ S.Symbol k, S.toSExpr v]) (Map.toList fs))
  toSExpr AnyT = S.A (S.Symbol "Any#")
  toSExpr VoidT = S.A (S.Symbol "Void#")

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
