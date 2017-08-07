{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.Syntax where

import           Control.Monad.State (evalState)
import Data.List (intercalate)

type Name = String
data Type = AtomT String
          | TTree [Type]
  deriving (Eq, Show)

textType (AtomT s)         = s
textType (TTree xs) = "(" ++ unwords (map textType xs)  ++")"

data AST = Symbol Name
         | Int Integer
         | Float Double
         | Bool Bool
         | String String
         | Typed AST Type
         | List [AST]
         | Tree [AST]
  deriving (Eq, Show)

textAST (Symbol name) = name
textAST (Int i)       = show i
textAST (Float f)     = show f
textAST (Bool True)   = "#t"
textAST (Bool False)  = "#f"
textAST (String s)    = show s
textAST (Typed a t)   = textAST a ++ ":" ++ textType t
textAST (List xs)   = "[" ++ unwords (map textAST xs) ++ "]"
textAST (Tree [Symbol "quote", Symbol s]) = "'" ++ s
textAST (Tree xs) = "(" ++ unwords (map textAST xs) ++ ")"

sample1 = Tree [Symbol "def", Typed (Symbol "ans") (AtomT "Int"), Int 42]
sample2 = Typed (Tree [ Symbol "if"
                      , Tree [Symbol "==", Symbol "ans", Int 42]
                      , Tree [Symbol "quote", Symbol "yes"]
                      , Tree [Symbol "quote", Symbol "no"]]) (AtomT "Symbol")

sample3 = Typed (Tree [Symbol "def"
                      , Tree [Typed (Symbol "f") (AtomT "Int"), Typed (Symbol "x") (AtomT "Int")]
                      , Tree [Symbol "*", Symbol "x", Symbol "x"]]) (AtomT "Symbol")

sample4 = Typed (List [String "a", Tree [Symbol "quote", Symbol "b"]]) (TTree [AtomT "List", AtomT "Symbol"])
