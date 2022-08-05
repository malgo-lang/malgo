module Malgo.TypeCheck.TypeRep where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Binary.Instances.UnorderedContainers ()
import Data.Data (Data)
import qualified Data.HashMap.Strict as HashMap
import Koriel.Id
import Koriel.Pretty (Pretty (pPrint, pPrintPrec), braces, hsep, parens, punctuate, (<+>))
import Malgo.Prelude

-- | Primitive Types
data PrimT = Int32T | Int64T | FloatT | DoubleT | CharT | StringT
  deriving stock (Eq, Show, Ord, Generic, Data)

instance Binary PrimT

instance ToJSON PrimT

instance FromJSON PrimT

instance Hashable PrimT

instance Pretty PrimT where
  pPrint Int32T = "Int32#"
  pPrint Int64T = "Int64#"
  pPrint FloatT = "Float#"
  pPrint DoubleT = "Double#"
  pPrint CharT = "Char#"
  pPrint StringT = "String#"

type TypeVar = Id Kind

data Type
  = Prim PrimT
  | Universal TypeVar
  | Existential TypeVar
  | Forall TypeVar Type
  | DataCon (Id Kind)
  | Apply Type Type
  | Arrow Type Type
  | Tuple Int
  | Record (HashMap Text Type)
  | Ptr Type
  | Bottom
  deriving stock (Eq, Show, Ord, Generic, Data)

instance Binary Type

instance ToJSON Type

instance FromJSON Type

instance Hashable Type

splitApply :: Type -> Maybe (Type, [Type])
splitApply (Apply t1 t2) = do
  (t1', ts) <- splitApply t1
  Just (t1', ts <> [t2])
splitApply (DataCon con) = Just (DataCon con, [])
splitApply (Tuple n) = Just (Tuple n, [])
splitApply _ = Nothing

instance Pretty Type where
  pPrintPrec _ _ (Prim t) = pPrint t
  pPrintPrec _ _ (Universal t) = pPrint t
  pPrintPrec _ _ (Existential t) = "^" <> pPrint t
  pPrintPrec l d (Forall v t)
    | d > 5 = parens $ "forall" <+> pPrint v <> "." <+> pPrintPrec l 5 t
    | otherwise = "forall" <+> pPrint v <> "." <+> pPrintPrec l 5 t
  pPrintPrec _ _ (DataCon c) = pPrint c
  pPrintPrec l _ (splitApply -> Just (Tuple _, ts)) =
    parens $ hsep $ punctuate "," $ map (pPrintPrec l 0) ts
  pPrintPrec l d (splitApply -> Just (t, ts))
    | d > 10 = parens $ pPrintPrec l 11 t <+> hsep (map (pPrintPrec l 10) ts)
    | otherwise = pPrintPrec l 11 t <+> hsep (map (pPrintPrec l 10) ts)
  pPrintPrec _ _ (Apply _ _) = error "pPrintPrec: Apply (unreachable)"
  pPrintPrec l d (Arrow t1 t2)
    | d > 8 = parens $ pPrintPrec l 9 t1 <+> "->" <+> pPrintPrec l 8 t2
    | otherwise = pPrintPrec l 9 t1 <+> "->" <+> pPrintPrec l 8 t2
  pPrintPrec _ _ (Tuple _) = error "pPrintPrec: Tuple (unreachable)"
  pPrintPrec _ _ (Record kts) =
    braces $ hsep $ punctuate "," $ map (\(k, t) -> pPrint k <> ":" <+> pPrint t) $ HashMap.toList kts
  pPrintPrec l d (Ptr t)
    | d > 10 = parens $ "Ptr#" <+> pPrintPrec l 10 t
    | otherwise = "Ptr#" <+> pPrintPrec l 10 t
  pPrintPrec _ _ Bottom = "Bottom"

data Kind = Star | KArr Kind Kind
  deriving stock (Eq, Show, Ord, Generic, Data)

instance Binary Kind

instance ToJSON Kind

instance FromJSON Kind

instance Hashable Kind

instance Pretty Kind where
  pPrintPrec _ _ Star = "*"
  pPrintPrec l d (KArr k1 k2) =
    if d > 10
      then parens (pPrintPrec l 11 k1 <+> "->" <+> pPrintPrec l 10 k2)
      else pPrintPrec l 11 k1 <+> "->" <+> pPrintPrec l 10 k2
