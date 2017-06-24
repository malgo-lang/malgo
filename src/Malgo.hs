{-# LANGUAGE FlexibleContexts #-}
module Malgo where

import           Control.Monad.RWS
import qualified Data.Map          as Map
import           Symbol
-- import           Syntax

newtype MalgoM a =
  MalgoM {
  unMalgo :: RWST () () MEnv IO a
  }

data MEnv = MEnv { uniq_source :: !Int
                 , sym_table   :: Map.Map String Int
                 }

initMEnv = MEnv { uniq_source = 0
                , sym_table = Map.empty
                }

instance Monad MalgoM where
  return a = MalgoM (return a)
  (MalgoM m) >>= f = MalgoM $
    do a <- m
       unMalgo (f a)

instance (MonadState MEnv MalgoM) where


symbol :: String -> MalgoM Symbol
symbol name = do
  MEnv { uniq_source = u
       , sym_table = table } <- get
  let (sym, table1) = case Map.lookup name table of
                        Just i -> (MkSymbol (name, i), table)
                        Nothing -> let i = Map.size table
                                   in (MkSymbol (name, i), Map.insert name i table)
  put MEnv { uniq_source = u
           , sym_table = table1
           }
  return sym
