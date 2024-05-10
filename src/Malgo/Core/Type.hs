{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Malgo.Core.Type
  ( Tag (..),
    Con (..),
    Type (..),
    pattern (:->),
    HasType (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Map.Strict qualified as Map
import Data.Store (Store)
import Data.Store.TH
import Malgo.Id
import Malgo.Prelude
import Test.QuickCheck (Arbitrary (..), oneof)
import Test.QuickCheck.Instances.Text ()

{-
Constructors  C ::= <tag n>
-}
data Tag
  = Data {name :: Text}
  | Tuple
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)
  deriving anyclass (Hashable, ToJSON, FromJSON, Store)

instance Arbitrary Tag where
  arbitrary = oneof [Data <$> arbitrary, pure Tuple]

instance Pretty Tag where
  pretty (Data name) = pretty name
  pretty Tuple = "Tuple#"

data Con = Con {tag :: Tag, parameters :: [Type]}
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)
  deriving anyclass (Hashable, ToJSON, FromJSON)

instance Arbitrary Con where
  arbitrary = Con <$> arbitrary <*> arbitrary

instance Pretty Con where
  pretty (Con tag xs) = parens $ sep $ pretty tag : map pretty xs

-- TODO: クロージャを表す型を追加
-- ClosureT [Type] Type
-- あるいはknown関数を表す型
-- FuncT [Type] Type
data Type
  = FuncT {parameters :: [Type], returns :: Type}
  | Int32T
  | Int64T
  | FloatT
  | DoubleT
  | CharT
  | StringT
  | BoolT
  | SumT {constructors :: [Con]}
  | PtrT {inner :: Type}
  | RecordT {map :: Map Text Type}
  | AnyT
  | VoidT
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)
  deriving anyclass (Hashable, ToJSON, FromJSON)

{-# COMPLETE (:->), Int32T, Int64T, FloatT, DoubleT, CharT, StringT, BoolT, SumT, PtrT, RecordT, AnyT, VoidT #-}

pattern (:->) :: [Type] -> Type -> Type
pattern a :-> b = FuncT a b

instance Arbitrary Type where
  arbitrary =
    oneof
      [ (:->) <$> arbitrary <*> arbitrary,
        pure Int32T,
        pure Int64T,
        pure FloatT,
        pure DoubleT,
        pure CharT,
        pure StringT,
        pure BoolT,
        SumT <$> arbitrary,
        PtrT <$> arbitrary,
        RecordT <$> arbitrary,
        pure AnyT,
        pure VoidT
      ]

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
