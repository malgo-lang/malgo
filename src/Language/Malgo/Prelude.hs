{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
module Language.Malgo.Prelude
  ( module X
  , Name
  , Info(..)
  ) where

import           Control.Lens              as X hiding (op)
import           Control.Monad.Trans       as X
import           Data.String               as X (IsString (..))
import           Data.Text.Prettyprint.Doc as X hiding (group, (<>))
import           GHC.Exts                  as X (IsList (..))
import           Prelude                   as X (error)
import           Protolude                 as X hiding (Strict, Type, Typeable,
                                                 from, list, sourceColumn,
                                                 sourceLine, sym, to, toList,
                                                 uncons, unsnoc, (<&>), (<.>), trans)

type Name = Text

newtype Info = Info (Text, Int, Int)
  deriving (Show, Read, Pretty)

instance Eq Info where
  _ == _ = True
