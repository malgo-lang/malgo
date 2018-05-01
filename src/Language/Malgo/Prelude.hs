{-# LANGUAGE FlexibleContexts      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.Prelude
  ( module X
  , PrettyPrint(..)
  , sandbox
  , Name
  , fromName
  , Info(..)
  ) where

import           Control.Lens        as X (Getter, Lens, Setter, from, lens,
                                           over, set, to, view)
import           Control.Lens.At     as X (at)
import           Control.Monad.Trans as X
import           Data.Default        as X
import           Data.String         as X (IsString (..))
import           GHC.Exts            as X (IsList (..))
import           Prelude             as X (error)
import           Protolude           as X hiding (Type, Typeable, find, from,
                                           sourceColumn, sourceLine, sym, to,
                                           toList)
import qualified Text.PrettyPrint    as P

class PrettyPrint a where
  pretty :: a -> P.Doc

instance PrettyPrint Text where
  pretty x = P.text (toS x)

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
  deriving (Show, Read)

instance Default Text where
  def = mempty

instance Default Info where
  def = Info (toS "", def, def)

instance Eq Info where
  _ == _ = True

instance PrettyPrint Info where
  pretty (Info x) = P.text $ show x
