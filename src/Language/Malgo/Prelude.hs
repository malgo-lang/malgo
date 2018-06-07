{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.Prelude
  ( module X
  , sandbox
  , Name
  , fromName
  , Info(..)
  ) where

import           Control.Monad.Trans as X
import           Data.Text.Prettyprint.Doc as X hiding ((<>), group)
import           Data.String         as X (IsString (..))
import           GHC.Exts            as X (IsList (..))
import           Prelude             as X (error)
import           Protolude           as X hiding (Type, Typeable, sourceColumn,
                                           sourceLine, sym, toList, list)

sandbox :: MonadState s m => m a -> m a
sandbox action = do
  s <- get
  ret <- action
  put s
  pure ret

type Name = Text

fromName :: StringConv Name a => Name -> a
fromName = toS

newtype Info = Info (Text, Int, Int)
  deriving (Show, Read, Pretty)

instance Eq Info where
  _ == _ = True
