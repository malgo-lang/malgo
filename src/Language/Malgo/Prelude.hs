{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Malgo.Prelude
  ( module X
  , PrettyPrint(..)
  , Info(..)
  , HasDummy(..)
  , Dict(..)
  , sandbox
  ) where

import Prelude as X (error)
import Protolude as X hiding (Typeable, sourceLine, sourceColumn, find)
import Control.Monad.Trans as X
import Data.String as X (IsString(..))
import qualified Data.Map as Map
import qualified Text.PrettyPrint as P

class PrettyPrint a where
  pretty :: a -> P.Doc

instance PrettyPrint Text where
  pretty x = P.text (toS x)

newtype Info = Info (Text, Int, Int)
  deriving (Show, Read)

instance Eq Info where
  _ == _ = True

instance PrettyPrint Info where
  pretty (Info x) = P.text $ show x

class HasDummy a where
  dummy :: a

instance HasDummy Info where
  dummy = Info ("<dummy>", 0, 0)

class Dict m where
  member :: Ord k => k -> m k a -> Bool
  member k m = case lookup k m of
                 Just _ -> True
                 Nothing -> False

  notMember :: Ord k => k -> m k a -> Bool
  notMember k m = not (member k m)

  lookup :: Ord k => k -> m k a -> Maybe a

  insert :: Ord k => k -> a -> m k a -> m k a

  fromList :: Ord k => [(k, a)] -> m k a

instance Dict Map where
  member = Map.member
  notMember = Map.notMember
  lookup = Map.lookup
  insert = Map.insert
  fromList = Map.fromList

sandbox :: MonadState s m => m a -> m (a, s)
sandbox action = do
  s <- get
  ret <- action
  put s
  return (ret, s)
