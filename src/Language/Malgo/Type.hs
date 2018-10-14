{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.Type
  ( TypeScheme(..)
  , TyRef
  , newTyRef
  , readTyRef
  , writeTyRef
  , Type(..)
  , TyCon(..)
  )
where

import           Language.Malgo.Id
import           Prelude           (show)
import           Universum         hiding (Type)

newtype TyRef = TyRef (IORef (Maybe Type))
  deriving Eq

newTyRef :: MonadIO f => f TyRef
newTyRef = TyRef <$> newIORef Nothing

readTyRef :: MonadIO m => TyRef -> m (Maybe Type)
readTyRef (TyRef r) = readIORef r

writeTyRef :: MonadIO m => TyRef -> Type -> m ()
writeTyRef (TyRef r) ty = do
  mty <- readIORef r
  case mty of
    Just _  -> pass
    Nothing -> writeIORef r (Just ty)

instance Show TyRef where
  show _ = "<meta>"

data TypeScheme = Forall [Id] Type
  deriving (Eq, Show)

data Type = TyApp TyCon [Type]
          | TyVar Id
          | TyMeta TyRef
  deriving (Eq, Show)

data TyCon = IntC Integer
           | Float32C
           | Float64C
           | ArrowC
           | FoldedC Id -- ^ 型環境を見て適切に展開される
           | RecordC [Text]
           | VariantC [Text]
           | TyFun [Id] Type
  deriving (Eq, Show)
