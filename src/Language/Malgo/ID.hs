{-# LANGUAGE TemplateHaskell #-}
module Language.Malgo.ID
  ( ID(..), RawID, name, uniq, meta ) where

import Language.Malgo.Type
import           Language.Malgo.Prelude
import qualified Text.PrettyPrint       as P
import Control.Lens

data ID a = ID { _name :: Name, _uniq :: Int, _meta :: a }
  deriving (Show, Ord, Read)


type RawID = ID ()

instance Eq (ID a) where
  x == y = _uniq x == _uniq y

makeLenses ''ID

instance Outputable a => Outputable (ID a) where
  ppr (ID n u m) = ppr n <> P.text "." <> P.int u <> P.braces (ppr m)

instance Typeable a => Typeable (ID a) where
  typeOf i = typeOf $ i ^. meta
