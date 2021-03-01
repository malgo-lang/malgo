{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Koriel.Core.Type where

import Data.Aeson
import Data.Aeson.Types (prependFailure, unexpected)
import Data.Binary (Binary)
import Koriel.Id
import Koriel.Prelude hiding ((.=))
import Koriel.Pretty

{-
Constructors  C ::= <tag n>
-}
type Tag = String

data Con = Con Tag [Type]
  deriving stock (Eq, Show, Ord, Generic)

instance Binary Con

instance Pretty Con where
  pPrint (Con tag xs) = "<" <> text tag <+> sep (punctuate "," (map pPrint xs)) <> ">"

instance ToJSON Con where
  toJSON (Con tag ts) = object [("type", "Con"), "tag" .= tag, "params" .= ts]

instance FromJSON Con where
  parseJSON = withObject "Con" $ \v -> do
    ty <- v .: "type"
    if ty == ("Con" :: String)
      then Con <$> v .: "tag" <*> v .: "params"
      else prependFailure "parsing Con failed, " (unexpected $ Object v)

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
  deriving stock (Eq, Show, Ord, Generic)

instance Binary Type

makePrisms ''Type

instance Pretty Type where
  pPrint (a :-> b) = brackets (sep $ punctuate "," $ map pPrint a) <+> "->" <+> pPrint b
  pPrint Int32T = "Int32#"
  pPrint Int64T = "Int64#"
  pPrint FloatT = "Float#"
  pPrint DoubleT = "Double#"
  pPrint CharT = "Char#"
  pPrint StringT = "String#"
  pPrint BoolT = "Bool#"
  pPrint (SumT cs) = braces $ sep (map pPrint $ toList cs)
  pPrint (PtrT t) = parens $ "Ptr#" <+> pPrint t
  pPrint AnyT = "*"
  pPrint VoidT = "Void"

instance ToJSON Type where
  toJSON (ps :-> r) = object [("type", "FunT"), "params" .= ps, "result" .= r]
  toJSON Int32T = object [("type", "Int32T")]
  toJSON Int64T = object [("type", "Int64T")]
  toJSON FloatT = object [("type", "FloatT")]
  toJSON DoubleT = object [("type", "DoubleT")]
  toJSON CharT = object [("type", "CharT")]
  toJSON StringT = object [("type", "StringT")]
  toJSON BoolT = object [("type", "BoolT")]
  toJSON (SumT cs) = object [("type", "SumT"), "con_set" .= cs]
  toJSON (PtrT t) = object [("type", "PtrT"), "ptr_to" .= t]
  toJSON AnyT = object [("type", "AnyT")]
  toJSON VoidT = object [("type", "VoidT")]

instance FromJSON Type where
  parseJSON = withObject "Type" $ \v -> do
    ty <- v .: "type"
    if
        | ty == ("FunT" :: String) -> (:->) <$> v .: "params" <*> v .: "result"
        | ty == "Int32T" -> pure Int32T
        | ty == "Int64T" -> pure Int64T
        | ty == "FloatT" -> pure FloatT
        | ty == "DoubleT" -> pure DoubleT
        | ty == "CharT" -> pure CharT
        | ty == "StringT" -> pure StringT
        | ty == "BoolT" -> pure BoolT
        | ty == "SumT" -> SumT <$> v .: "con_set"
        | ty == "PtrT" -> PtrT <$> v .: "ptr_to"
        | ty == "AnyT" -> pure AnyT
        | ty == "VoidT" -> pure VoidT
        | otherwise -> prependFailure "parsing Type failed, " (unexpected $ Object v)

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
