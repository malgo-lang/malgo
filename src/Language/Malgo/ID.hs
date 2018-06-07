{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Language.Malgo.ID
  ( ID(..), RawID, idName, idUniq, idMeta ) where

import           Control.Lens
import           Language.Malgo.Prelude
import           Language.Malgo.Type

data ID a = ID { _idName :: Name, _idUniq :: Int, _idMeta :: a }
  deriving (Show, Ord, Read)

type RawID = ID ()

instance Eq (ID a) where
  x == y = _idUniq x == _idUniq y

makeLenses ''ID

instance Pretty a => Pretty (ID a) where
  pretty (ID n u m) =
    pretty n <> "." <> pretty u <> braces (pretty m)

instance Typeable a => Typeable (ID a) where
  typeOf i = typeOf $ i ^. idMeta
