{-# LANGUAGE DeriveAnyClass #-}

module Koriel.Core.Syntax.Unboxed (Unboxed (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Data (Data)
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import Koriel.Core.Type
import Koriel.Prelude
import Koriel.Pretty
import Numeric (showHex)

-- | unboxed values
data Unboxed
  = Int32 Integer
  | Int64 Integer
  | Float Float
  | Double Double
  | Char Char
  | String Text
  | Bool Bool
  deriving stock (Eq, Ord, Show, Generic, Data, Typeable)
  deriving anyclass (Binary, ToJSON, FromJSON)

instance HasType Unboxed where
  typeOf Int32 {} = Int32T
  typeOf Int64 {} = Int64T
  typeOf Float {} = FloatT
  typeOf Double {} = DoubleT
  typeOf Char {} = CharT
  typeOf String {} = StringT
  typeOf Bool {} = BoolT

instance Pretty Unboxed where
  pPrint (Int32 x) = pPrint x <> "_i32"
  pPrint (Int64 x) = pPrint x <> "_i64"
  pPrint (Float x) = text (showHex (castFloatToWord32 x) "") <> "_f32" <+> "#|" <> pPrint x <> "|#"
  pPrint (Double x) = text (showHex (castDoubleToWord64 x) "") <> "_f64" <+> "#|" <> pPrint x <> "|#"
  pPrint (Char x) = quotes (text $ convertString $ showLitChar x "")
  pPrint (String x) = doubleQuotes (text $ concatMap (`showLitChar` "") $ convertString @_ @String x)
  pPrint (Bool True) = "True#"
  pPrint (Bool False) = "False#"
