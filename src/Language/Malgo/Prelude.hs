{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.Prelude
  ( module X
  , Outputable(..)
  , sandbox
  , Name
  , fromName
  , Info(..)
  ) where

import           Control.Monad.Trans as X
import           Data.Default        as X
import           Data.String         as X (IsString (..))
import           GHC.Exts            as X (IsList (..))
import           Prelude             as X (error)
import           Protolude           as X hiding (Type, Typeable, sourceColumn,
                                           sourceLine, sym, toList)
import qualified Text.PrettyPrint    as P

class Outputable a where
  ppr :: a -> P.Doc

  pprList :: [a] -> P.Doc
  pprList xs = P.sep (P.punctuate "," $ map ppr xs)

instance Outputable Text where
  ppr x = P.text (toS x)

instance Outputable a => Outputable [a] where
  ppr x = pprList x

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
  def = Info ("", def, def)

instance Eq Info where
  _ == _ = True

instance Outputable Info where
  ppr (Info x) = P.text $ show x
