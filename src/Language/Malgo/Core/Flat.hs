{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Core.Flat
  ( Flat,
  )
where

import Language.Malgo.IR.Core
import Language.Malgo.Id
import Language.Malgo.Pass
import Language.Malgo.Prelude
import Language.Malgo.TypeRep.CType

data Flat

instance Pass Flat (Exp (Id CType)) (Exp (Id CType)) where
  passName = "flat"
  trans e = uncurry (flip appEndo) <$> runWriterT (flatExp e)

flatExp :: Monad m => Exp a -> WriterT (Endo (Exp a)) m (Exp a)
flatExp (Let ds e) = do
  ds <- traverse (rtraverse flatObj) ds
  tell $ Endo $ \k -> Let ds k
  flatExp e
flatExp (Match v (Unpack con ps e :| [])) = do
  v <- flatExp v
  tell $ Endo $ \k -> Match v (Unpack con ps k :| [])
  flatExp e
flatExp (Match v (Bind x e :| [])) = do
  v <- flatExp v
  tell $ Endo $ \k -> Match v (Bind x k :| [])
  flatExp e
flatExp (Match e cs) = do
  e <- flatExp e
  cs <- traverse flatCase cs
  pure $ Match e cs
flatExp e = pure e

flatObj :: Monad m => Obj a -> m (Obj a)
flatObj (Fun ps e) = do
  e <- uncurry (flip appEndo) <$> runWriterT (flatExp e)
  pure $ Fun ps e
flatObj o = pure o

flatCase :: Monad m => Case a -> m (Case a)
flatCase (Unpack con ps e) = do
  e <- uncurry (flip appEndo) <$> runWriterT (flatExp e)
  pure $ Unpack con ps e
flatCase (Bind x e) = do
  e <- uncurry (flip appEndo) <$> runWriterT (flatExp e)
  pure $ Bind x e
