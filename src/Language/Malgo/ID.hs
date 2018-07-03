{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.Malgo.ID
  (ID(..), RawID, idName, idUniq, idMeta, newID) where

import           Control.Lens
import           Language.Malgo.Monad
import           Language.Malgo.Prelude
import           Language.Malgo.Type
import           RIO                    (RIO)

data ID a = ID { _idName :: Name, _idUniq :: Int, _idMeta :: a }
  deriving (Show, Ord, Read)

type RawID = ID ()

instance Eq (ID a) where
  x == y = _idUniq x == _idUniq y

makeLenses ''ID

instance Pretty a => Pretty (ID a) where
  pretty (ID n u _) =
    pretty n <> "." <> pretty u

instance Typeable a => Typeable (ID a) where
  typeOf i = typeOf $ i ^. idMeta

newID :: MonadMalgo s m => Name -> a -> m (ID a)
newID name ty = do
  u <- newUniq
  return $ ID name u ty
