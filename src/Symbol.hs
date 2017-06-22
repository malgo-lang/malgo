{-# LANGUAGE FlexibleContexts #-}
module Symbol (symbol, Symbol, empty) where

import           Control.Monad.State
import qualified Data.Map            as Map

newtype Symbol = MkSymbol (String, Int)
  deriving Show

instance Eq Symbol where
  (MkSymbol (_, i)) == (MkSymbol (_, j)) = i == j

instance Ord Symbol where
  (MkSymbol (_, i)) <= (MkSymbol (_, j)) = i <= j

symbol' name table =
  case Map.lookup name table of
    Just i -> (MkSymbol (name, i), table)
    Nothing -> let i = Map.size table
               in (MkSymbol (name, i), Map.insert name i table)

symbol :: String -> State (Map.Map String Int) Symbol
symbol name = do
  table <- get
  let (sym, table1) = symbol' name table
  put table1
  return sym

test' = do
  x <- symbol "x"
  y <- symbol "y"
  return [x, y]

test = runState test' empty

empty = Map.empty
