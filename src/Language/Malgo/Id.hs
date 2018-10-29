{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.Id where

import           Data.Outputable
import           Language.Malgo.Monad
import           Language.Malgo.Pretty
import           Universum

data Id = Id { _name :: Text, _uniq :: Int }
    deriving (Show, Eq, Ord, Generic)

newId :: MonadMalgo f => Text -> f Id
newId name = Id name <$> newUniq

instance Outputable Id

instance Pretty Id where
  pPrint (Id name uniq) = pPrint name <> "_" <> pPrint uniq
