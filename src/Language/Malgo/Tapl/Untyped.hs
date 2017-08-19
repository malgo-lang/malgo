{-# LANGUAGE FlexibleContexts #-}
module Language.Malgo.Tapl.Untyped where

import           Control.Monad.Identity
import           Data.List              (elemIndex)
import           Language.Malgo.Parser
import           Language.Malgo.Syntax

data Term = Var Name
          | Abs Name Term
          | App Term Term
          deriving (Eq, Show)

trans (Tree [Symbol "^", Symbol x, body]) = fmap (Abs x) (trans body)
trans (Tree [x]) = trans x
trans (Tree (x:rest)) = liftM2 App (trans x) (trans (Tree rest))
trans (Symbol x) = return $ Var x

parseLambda src = case parse src of
  Right ast -> Right $ runIdentity (trans ast)
  Left msg  -> Left msg
