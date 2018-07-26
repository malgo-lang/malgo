{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.Malgo.ID
  (ID(..), RawID, TypedID, idName, idUniq, idMeta, newID) where

import           Language.Malgo.Pretty
import           Language.Malgo.Monad
import           Language.Malgo.Type
import           Lens.Micro.Platform       (makeLenses)
import           RIO                       hiding (Typeable)

data ID a = ID { _idName :: Text, _idUniq :: Int, _idMeta :: a }
  deriving (Show, Ord, Read)

type RawID = ID ()

type TypedID = ID Type

instance Eq (ID a) where
  x == y = _idUniq x == _idUniq y

makeLenses ''ID

ignore :: a -> b -> b
ignore = flip const

instance Pretty a => Pretty (ID a) where
  pPrint (ID n u m) =
#ifdef SHOW_META
    pPrint n <> "." <> pPrint u <> braces (pPrint m)
#else
    ignore m $ pPrint n <> "." <> pPrint u
#endif

instance Typeable a => Typeable (ID a) where
  typeOf i = typeOf $ view idMeta i

newID :: MonadMalgo f => a -> Text -> f (ID a)
newID m n =
  ID n <$> newUniq <*> pure m
