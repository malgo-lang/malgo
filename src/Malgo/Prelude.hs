{-# LANGUAGE TypeFamilies #-}

module Malgo.Prelude
  ( module X,
    show,
    pShow,
  )
where

import Data.Map as X (Map)
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