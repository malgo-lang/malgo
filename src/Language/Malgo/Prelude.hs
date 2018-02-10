{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.Prelude
  ( module X
  , PrettyPrint(..)
  , HasDummy(..)
  , Dict(..)
  ) where

import Protolude as X hiding (Typeable)
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
  pretty (Info x) = P.text . show $ x

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

instance Dict Map where
  member = Map.member
  notMember = Map.notMember
  lookup = Map.lookup
  insert = Map.insert
