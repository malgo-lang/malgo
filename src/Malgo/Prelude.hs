{-# LANGUAGE TypeFamilies #-}

module Malgo.Prelude
  ( module X,
    show,
    ppShow,
  )
where

import Data.Map as X (Map)
import Data.String.Conversions as X (ConvertibleStrings, convertString)
import Data.Text as X (Text)
import Effectful as X
import Text.Show.Pretty qualified as Pretty
import Prelude as X hiding (log, show, lookup)
import Prelude qualified

show :: (Show a, ConvertibleStrings String s) => a -> s
show = convertString . Prelude.show

ppShow :: (Show a, ConvertibleStrings String s) => a -> s
ppShow = convertString . Pretty.ppShow