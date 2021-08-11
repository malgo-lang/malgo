{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Koriel.Pretty
  ( module Prettyprinter,
    (<+>),
    maybeParens,
    errorDoc,
    toText,
  )
where

import Data.Fix
import Koriel.Prelude
import Prettyprinter hiding ((<+>))
import qualified Prettyprinter as P
import qualified Prelude
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Data.Text.Prettyprint.Doc.Render.String (renderString)

-- change operator precedence
infixl 9 <+>
(<+>) :: Doc ann -> Doc ann -> Doc ann
(<+>) = (P.<+>)

maybeParens :: Bool -> Doc ann -> Doc ann
maybeParens True x = parens x
maybeParens False x = x

toText :: (Profunctor p, Contravariant f, Pretty a) => Optic' p f a Text
toText = to (renderStrict . layoutSmart defaultLayoutOptions . pretty)

errorDoc ::
#ifdef DEBUG
  HasCallStack => Doc ann -> a
errorDoc x = Prelude.error $ renderString $ layoutSmart defaultLayoutOptions x
#else
  Doc ann -> a
errorDoc x = Prelude.errorWithoutStackTrace $ renderString $ layoutSmart defaultLayoutOptions x
#endif

instance Pretty (f (Fix f)) => Pretty (Fix f) where
  pretty (Fix f) = pretty f
