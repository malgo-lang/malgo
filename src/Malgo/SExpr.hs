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
  sShow = convertString . S.encodeOne (S.unconstrainedPrint atomToText & S.setIndentStrategy indentStrategy) . toSExpr

instance ToSExpr Text where
  toSExpr = S.A . Symbol

instance (ToSExpr a, ToSExpr b) => ToSExpr (a, b) where
  toSExpr (a, b) = S.L [toSExpr a, toSExpr b]

instance (ToSExpr a, ToSExpr b, ToSExpr c) => ToSExpr (a, b, c) where
  toSExpr (a, b, c) = S.L [toSExpr a, toSExpr b, toSExpr c]

instance (ToSExpr a) => ToSExpr [a] where
  toSExpr = S.L . map toSExpr
  sShow = convertString . S.encode (S.unconstrainedPrint atomToText & S.setIndentStrategy indentStrategy) . map toSExpr

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

indentStrategy :: S.SExpr Atom -> S.Indent
indentStrategy (S.A (Symbol "def")) = S.SwingAfter 4
indentStrategy (S.A (Symbol "$")) = S.SwingAfter 2
indentStrategy (S.A (Symbol ".")) = S.SwingAfter 2
indentStrategy (S.A (Symbol "=")) = S.SwingAfter 2
indentStrategy (S.A (Symbol "do")) = S.SwingAfter 2
indentStrategy (S.A (Symbol "then")) = S.SwingAfter 2
indentStrategy (S.A (Symbol "prim")) = S.SwingAfter 2
indentStrategy (S.A (Symbol "invoke")) = S.SwingAfter 2
indentStrategy (S.A (Symbol "sum")) = S.SwingAfter 2
indentStrategy _ = S.Swing
