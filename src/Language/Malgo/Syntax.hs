{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.Syntax where

import           Data.List       (intercalate)
import           Text.Parsec.Pos (SourcePos, newPos)

type Name = String

data AST = Symbol SourcePos Name
         | Int SourcePos Integer
         | Float SourcePos Double
         | Bool SourcePos Bool
         | Char SourcePos Char
         | String SourcePos String
         | Typed SourcePos AST AST
         | List SourcePos [AST]
         | Tree [AST]
  deriving (Show)

instance Eq AST where
  (Symbol _ a) == (Symbol _ b) = a == b
  (Int _ a) == (Int _ b) = a == b
  (Float _ a) == (Float _ b) = a == b
  (Bool _ a) == (Bool _ b) = a == b
  (Char _ a) == (Char _ b) = a == b
  (String _ a) == (String _ b) = a == b
  (Typed _ a b) == (Typed _ c d) = (a == c) && (b == d)
  (List _ xs) == (List _ ys) = xs == ys
  (Tree xs) == (Tree ys) = xs == ys

dummyPos = newPos "" 0 0

textAST (Symbol _ name) = name
textAST (Int _ i)       = show i
textAST (Float _ f)     = show f
textAST (Bool _ True)   = "#t"
textAST (Bool _ False)  = "#f"
textAST (Char _ c)      = show c
textAST (String _ s)    = show s
textAST (Typed _ a t)   = textAST a ++ ":" ++ textAST t
textAST (List _ xs)     = "[" ++ unwords (map textAST xs) ++ "]"
textAST (Tree [Symbol _ "quote", Symbol _ s]) = "'" ++ s
textAST (Tree xs)     = "(" ++ unwords (map textAST xs) ++ ")"

sample1 = Tree [Symbol dummyPos "def", Typed dummyPos (Symbol dummyPos "ans") (Symbol dummyPos "Int"), Int dummyPos 42]
sample2 = Typed dummyPos (Tree [ Symbol dummyPos "if"
                               , Tree [Symbol dummyPos "==", Symbol dummyPos "ans", Int dummyPos 42]
                               , Tree [Symbol dummyPos  "quote", Symbol dummyPos "yes"]
                               , Tree [Symbol dummyPos  "quote", Symbol dummyPos "no"]]) (Symbol dummyPos "Symbol")

sample3 = Typed dummyPos (Tree [Symbol dummyPos  "def"
                               , Tree [Typed dummyPos (Symbol dummyPos "f") (Symbol dummyPos "Int"), Typed dummyPos (Symbol dummyPos "x") (Symbol dummyPos "Int")]
                               , Tree [Symbol dummyPos "*", Symbol dummyPos "x", Symbol dummyPos "x"]]) (Symbol dummyPos "Symbol")

sample4 = Typed dummyPos (List dummyPos [Char dummyPos 'c', Tree [Symbol dummyPos "quote", Symbol dummyPos "b"]]) (Tree [Symbol dummyPos "List", Symbol dummyPos "Symbol"])
