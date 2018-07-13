{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.Malgo.ID
  (ID(..), RawID, TypedID, idName, idUniq, idMeta, newID) where

import           Control.Lens.TH
import           Data.Text.Prettyprint.Doc
import           Language.Malgo.Monad
import           Language.Malgo.Type
import           RIO                       hiding (Typeable)

data ID a = ID { _idName :: Text, _idUniq :: Int, _idMeta :: a }
  deriving (Show, Ord, Read)

type RawID = ID ()

type TypedID = ID Type

instance Eq (ID a) where
  x == y = _idUniq x == _idUniq y

makeLenses ''ID

instance Pretty a => Pretty (ID a) where
  pretty (ID n u m) =
    pretty n <> "." <> pretty u <> braces (pretty m)

instance Typeable a => Typeable (ID a) where
  typeOf i = typeOf $ view idMeta i

newID :: MonadMalgo f => a -> Text -> f (ID a)
newID m n =
  ID n <$> newUniq <*> pure m
