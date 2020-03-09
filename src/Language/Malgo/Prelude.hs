{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Malgo.Prelude
  ( module Relude
  , module Relude.Extra.Map
  , module Relude.Extra.Tuple
  , module Relude.Extra.Bifunctor
  , module Control.Monad.Trans.Writer.CPS
  , module Control.Monad.Writer.Class
  , module Control.Lens.Cons
  , Complement(..)
  , localState
  , unzip
  , ltraverse
  , rtraverse
  , DiffList
  , Unreachable(..)
  )
where

import           Relude                  hiding ( Constraint
                                                , Type
                                                , Op
                                                , init
                                                , unzip
                                                , pass
                                                , return
                                                , uncons
                                                )
import           Relude.Extra.Map        hiding ( size
                                                , delete
                                                )
import           Relude.Extra.Tuple
import           Relude.Extra.Bifunctor
import qualified Data.Set                      as Set
import qualified Data.List                     as List
import           Control.Monad.Trans.Writer.CPS ( WriterT
                                                , runWriterT
                                                )
import qualified Control.Monad.Trans.Writer.CPS
                                               as W
import           Control.Monad.Writer.Class
import           Control.Lens.Cons
import           Control.Lens.Prism             ( prism )
import           GHC.Exts                       ( Item
                                                , toList
                                                )
import           Text.PrettyPrint.HughesPJClass ( Pretty(..)
                                                , text
                                                )
import           Text.Parsec.Pos                ( SourcePos )

-- 差分を取ることができるデータ構造を表す型クラス
class One a => Complement a where
  (\\) :: a -> a -> a

  delete :: OneItem a -> a -> a
  delete x s = s \\ one x
  {-# INLINE delete #-}

instance Ord a => Complement (Set a) where
  (\\) = (Set.\\)
  {-# INLINE (\\) #-}
  delete = Set.delete
  {-# INLINE delete #-}

instance Eq a => Complement [a] where
  (\\) = (List.\\)
  {-# INLINE (\\) #-}
  delete = List.delete
  {-# INLINE delete #-}

-- Stateモナドのヘルパー。ローカルな状態を表現する
localState :: MonadState s m => m a -> m a
localState m = do
  backup <- get
  v      <- m
  put backup
  pure v
{-# INLINE localState #-}

-- unzip :: [(a, b)] -> ([a], [b]) の一般化
unzip :: Functor f => f (a, b) -> (f a, f b)
unzip xs = (fst <$> xs, snd <$> xs)
{-# INLINE unzip #-}

-- bitraverseの部分適用
ltraverse :: (Bitraversable t, Applicative f) => (a -> f c) -> t a d -> f (t c d)
ltraverse f = bitraverse f pure
{-# INLINE ltraverse #-}

rtraverse :: (Bitraversable t, Applicative f) => (b -> f c) -> t a b -> f (t a c)
rtraverse = bitraverse pure
{-# INLINE rtraverse #-}

-- 差分リスト
newtype DiffList a = DiffList (Endo [a])
  deriving newtype (Semigroup, Monoid)

instance One (DiffList a) where
  type OneItem (DiffList a) = a
  one x = DiffList (Endo (x :))
  {-# INLINE one #-}

instance Cons (DiffList a) (DiffList b) a b where
  _Cons = prism (\(x, xs) -> one x <> xs) $ \(DiffList xxs) -> case appEndo xxs [] of
    (x : xs) -> Right (x, DiffList (Endo (xs <>)))
    []       -> Left mempty
  {-# INLINE _Cons #-}

instance Snoc (DiffList a) (DiffList b) a b where
  _Snoc = prism (\(xs, x) -> xs <> one x) $ \(DiffList xxs) -> case appEndo xxs [] of
    []   -> Left mempty
    list -> Right (DiffList $ Endo (List.init list <>), List.last list)
  {-# INLINE _Snoc #-}

instance IsList (DiffList a) where
  type Item (DiffList a) = a
  fromList xs = DiffList $ Endo (xs <>)
  {-# INLINE fromList #-}
  toList (DiffList xs) = appEndo xs []
  {-# INLINE toList #-}

instance Functor DiffList where
  fmap f (DiffList xs) = DiffList $ Endo (map f (appEndo xs []) <>)
  {-# INLINE fmap #-}

instance Foldable DiffList where
  foldMap f (DiffList xs) = foldMap f (appEndo xs [])
  {-# INLINE foldMap #-}
  foldr f a (DiffList xs) = foldr f a (appEndo xs [])
  {-# INLINE foldr #-}
  toList = GHC.Exts.toList
  {-# INLINE toList #-}

-- mtlのインスタンスの追加定義
instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
  writer = W.writer
  tell   = W.tell
  listen = W.listen
  pass   = W.pass
  {-# INLINE writer #-}
  {-# INLINE tell #-}
  {-# INLINE listen #-}
  {-# INLINE pass #-}

instance (Monoid w, MonadReader r m) => MonadReader r (WriterT w m) where
  ask    = lift ask
  local  = W.mapWriterT . local
  reader = lift . reader
  {-# INLINE ask #-}
  {-# INLINE local #-}
  {-# INLINE reader #-}

instance (Monoid w, MonadState s m) => MonadState s (WriterT w m) where
  get   = lift get
  put   = lift . put
  state = lift . state
  {-# INLINE get #-}
  {-# INLINE put #-}
  {-# INLINE state #-}

-- Unreachable
data Unreachable = Unreachable
  deriving stock (Show, Typeable)

instance Exception Unreachable

-- Pretty SourcePos
instance Pretty SourcePos where
  pPrint = text . show
