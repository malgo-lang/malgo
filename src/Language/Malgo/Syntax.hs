{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.Syntax where

import           Control.Monad.State (evalState)
import Data.List (intercalate)

type Name = String
data Type = UnitT
          | IntT
          | BoolT
          | FloatT
          | SymbolT
          | StringT
          | ListT Type
          | FunT Type Type
  deriving (Eq, Show)

textType UnitT             = "Unit"
textType IntT              = "Int"
textType BoolT             = "Bool"
textType FloatT            = "Float"
textType SymbolT           = "Symbol"
textType StringT           = "String"
textType (ListT t)         = "List (" ++ textType t ++ ")"
textType (FunT p ret)    = textType p ++ " -> " ++ textType ret

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
textAST (Tree xs) = "(" ++ unwords (map textAST xs) ++ ")"

sample1 = Tree [Symbol "def", Typed (Symbol "ans") IntT, Int 42]
sample2 = Typed (Tree [ Symbol "if"
                      , Tree [Symbol "==", Symbol "ans", Int 42]
                      , Tree [Symbol "quote", Symbol "yes"]
                      , Tree [Symbol "quote", Symbol "no"]]) SymbolT

sample3 = Typed (Tree [Symbol "def"
                      , Tree [Typed (Symbol "f") IntT, Typed (Symbol "x") IntT]
                      , Tree [Symbol "*", Symbol "x", Symbol "x"]]) SymbolT

sample4 = Typed (List [String "a", Symbol "b"]) (ListT SymbolT)
