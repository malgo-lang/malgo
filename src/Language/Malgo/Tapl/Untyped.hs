{-# LANGUAGE FlexibleContexts #-}
module Language.Malgo.Tapl.Untyped where

import           Control.Monad.Identity
import           Data.List              (elemIndex)
import qualified Data.Map               as Map
import           Language.Malgo.Parser
import           Language.Malgo.Syntax
import           Text.Parsec.Pos

data Term = Var SourcePos Name
          | Abs SourcePos Name Term
          | App SourcePos Term Term
          deriving (Eq, Show)

prettyLambda (Tree _ [Symbol _ "lambda", Symbol _ name, body]) = "\\" ++ name ++ ". " ++ prettyLambda body
prettyLambda (Tree _ xs) = "(" ++ unwords (map prettyLambda xs) ++ ")"
prettyLambda (Symbol _ x) = x

type Env = Map.Map Name AST

transLambda (Tree _ [Symbol pos "lambda", Symbol _ name, body]) = Abs pos name (transLambda body)
transLambda (Tree _ [x]) = transLambda x
transLambda (Tree _ [x, y]) = App (getPos x) (transLambda x) (transLambda y)
transLambda (Tree _ (x:y:xs)) = App (getPos x) (App (getPos x) (transLambda x) (transLambda y)) (transLambda (Tree (getPos y) xs))
transLambda (Symbol pos x) = Var pos x
