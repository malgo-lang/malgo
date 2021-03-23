{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Koriel.Id
  ( ModuleName (..),
    _Module,
    Id,
    idName,
    idUniq,
    idMeta,
    idIsTopLevel,
    idIsExternal,
    newId,
    newGlobalId,
    newLocalId,
    newTopLevelId,
    pprIdName,
  )
where

import Data.Binary (Binary)
import Data.Deriving
import Data.Hashable (Hashable (hashWithSalt))
import Koriel.MonadUniq
import Koriel.Prelude hiding (toList, (.=))
import Koriel.Pretty

newtype ModuleName = ModuleName String
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype (Pretty)

instance Binary ModuleName

_Module :: Lens' ModuleName String
_Module = lens (\(ModuleName s) -> s) (\_ s -> ModuleName s)

data Id a = Id
  { _idName :: String,
    _idUniq :: Int,
    _idMeta :: a,
    _idIsTopLevel :: Bool,
    _idIsExternal :: Bool
  }
  deriving stock (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Generic)

deriveEq1 ''Id
deriveOrd1 ''Id
deriveShow1 ''Id

-- TODO: calculate hash from idUniq
instance Hashable (Id a) where
  hashWithSalt salt Id {_idUniq} = hashWithSalt salt _idUniq

instance Binary a => Binary (Id a)

pprIdName :: Id a -> Doc
pprIdName Id {_idName} = text _idName

#ifdef DEBUG
instance Pretty a => Pretty (Id a) where
  pPrint (Id n _ m _ True) = text n <> braces (pPrint m)
  pPrint (Id n u m _ False) = text n <> "_" <> text (show u) <> braces (pPrint m)

instance Pretty1 Id where
  liftPPrintPrec ppr l d (Id n _ m _ True) = text n <> braces (ppr l d m)
  liftPPrintPrec ppr l d (Id n u m _ False) = text n <> "_" <> text (show u) <> braces (ppr l d m)
#else
instance Pretty a => Pretty (Id a) where
  pPrint Id {_idName, _idUniq, _idIsExternal = False} = text _idName <> "_" <> pPrint _idUniq
  pPrint Id {_idName, _idIsExternal = True} = text _idName

instance Pretty1 Id where
  liftPPrintPrec _ _ _ Id {_idName, _idUniq, _idIsExternal = False} = text _idName <> "_" <> pPrint _idUniq
  liftPPrintPrec _ _ _ Id {_idName, _idIsExternal = True} = text _idName
#endif

makeLenses ''Id

newId :: MonadUniq f => String -> a -> Bool -> Bool -> f (Id a)
newId n m t e = Id n <$> getUniq <*> pure m <*> pure t <*> pure e

newLocalId :: MonadUniq f => String -> a -> f (Id a)
newLocalId n m = Id n <$> getUniq <*> pure m <*> pure False <*> pure False

newTopLevelId :: MonadUniq f => String -> a -> f (Id a)
newTopLevelId n m = Id n <$> getUniq <*> pure m <*> pure True <*> pure False

newGlobalId :: MonadUniq f => String -> a -> f (Id a)
newGlobalId n m = Id n <$> getUniq <*> pure m <*> pure True <*> pure True
