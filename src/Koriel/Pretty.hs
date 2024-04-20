{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Koriel.Pretty
  ( module Prettyprinter,
    errorDoc,
    render,
    maybeParens,
  )
where

import Koriel.Prelude
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Prettyprinter.Render.Text (renderStrict)
import Text.Megaparsec.Pos qualified as Megaparsec
import Prelude qualified

errorDoc :: (HasCallStack) => Doc x -> a
errorDoc x = Prelude.error $ renderString $ layoutSmart defaultLayoutOptions x

-- Pretty SourcePos
instance Pretty Megaparsec.SourcePos where
  pretty = pretty . convertString @_ @Text . Megaparsec.sourcePosPretty

render :: Doc ann -> Text
render = renderStrict . layoutSmart defaultLayoutOptions

maybeParens :: Bool -> Doc ann -> Doc ann
maybeParens True = parens
maybeParens False = identity