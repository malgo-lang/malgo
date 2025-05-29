module Malgo.SExpr
  ( ToSExpr (..),
    Atom (..),
  )
where

import Data.SCargot qualified as S
import Data.SCargot.Repr qualified as S
import Data.SCargot.Repr.Basic qualified as S
import Malgo.Prelude

class ToSExpr a where
  toSExpr :: a -> S.SExpr Atom
  sShow :: (ConvertibleStrings Text s) => a -> s
  sShow = convertString . S.encodeOne (S.basicPrint atomToText) . toSExpr

instance ToSExpr Void where
  toSExpr = absurd

instance ToSExpr Text where
  toSExpr = S.A . Symbol

instance ToSExpr Int where
  toSExpr = S.A . (`Int` Nothing) . fromIntegral

instance (ToSExpr a, ToSExpr b) => ToSExpr (a, b) where
  toSExpr (a, b) = S.L [toSExpr a, toSExpr b]

instance (ToSExpr a, ToSExpr b, ToSExpr c) => ToSExpr (a, b, c) where
  toSExpr (a, b, c) = S.L [toSExpr a, toSExpr b, toSExpr c]

instance (ToSExpr a, ToSExpr b, ToSExpr c, ToSExpr d) => ToSExpr (a, b, c, d) where
  toSExpr (a, b, c, d) = S.L [toSExpr a, toSExpr b, toSExpr c, toSExpr d]

instance (ToSExpr a) => ToSExpr [a] where
  toSExpr = S.L . map toSExpr
  sShow =
    convertString
      . S.encode (S.basicPrint atomToText)
      . map toSExpr

data Atom
  = Symbol Text
  | Int Integer (Maybe Text)
  | Float Float
  | Double Double
  | Char Char
  | String Text
  deriving stock (Show, Eq)

instance IsString Atom where
  fromString = Symbol . fromString

atomToText :: Atom -> Text
atomToText (Symbol t) = t
atomToText (Int n Nothing) = convertString $ show n
atomToText (Int n (Just t)) = convertString (show n) <> "_" <> t
atomToText (Float n) = convertString $ show n <> "_f32"
atomToText (Double n) = convertString $ show n <> "_f64"
atomToText (Char c) = "'" <> convertString (showLitChar c "") <> "'"
atomToText (String t) = "\"" <> convertString (concatMap (`showLitChar` "") (convertString @_ @String t)) <> "\""
