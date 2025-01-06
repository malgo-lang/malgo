{-# LANGUAGE TypeFamilies #-}

module Malgo.Prelude
  ( module X,
    show,
    pShow,
    ToSExpr (..),
    Atom (..),
  )
where

import Data.Function ((&))
import Data.Map as X (Map)
import Data.SCargot qualified as S
import Data.SCargot.Repr qualified as S
import Data.SCargot.Repr.Basic qualified as S
import Data.String.Conversions as X (ConvertibleStrings, LazyText, convertString)
import Data.Text as X (Text)
import Effectful as X
import Text.Pretty.Simple qualified as Pretty
import Prelude as X hiding (log, lookup, show)
import Prelude qualified

show :: (Show a, ConvertibleStrings String s) => a -> s
show = convertString . Prelude.show

pShow :: (Show a, ConvertibleStrings LazyText s) => a -> s
pShow = convertString . Pretty.pShow

class ToSExpr a where
  toSExpr :: a -> S.SExpr Atom
  sShow :: (ConvertibleStrings Text s) => a -> s
  sShow = convertString . S.encodeOne (S.unconstrainedPrint atomToText & S.setIndentStrategy indentStrategy) . toSExpr

data Atom = Symbol Text | String Text | Number Int
  deriving (Show, Eq)

atomToText :: Atom -> Text
atomToText (Symbol t) = t
atomToText (String t) = "'" <> t <> "'"
atomToText (Number n) = show n

indentStrategy :: S.SExpr Atom -> S.Indent
indentStrategy (S.A (Symbol "def")) = S.SwingAfter 4
indentStrategy (S.A (Symbol "$")) = S.SwingAfter 2
indentStrategy (S.A (Symbol ".")) = S.SwingAfter 2
indentStrategy (S.A (Symbol "do")) = S.SwingAfter 2
indentStrategy (S.A (Symbol "then")) = S.SwingAfter 2
indentStrategy (S.A (Symbol "prim")) = S.SwingAfter 2
indentStrategy (S.A (Symbol "invoke")) = S.SwingAfter 2
indentStrategy _ = S.Swing

instance (ToSExpr a) => ToSExpr [a] where
  toSExpr = S.L . map toSExpr
  sShow = convertString . S.encode (S.unconstrainedPrint atomToText & S.setIndentStrategy indentStrategy) . map toSExpr
