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
          | ListT Type
          | VectorT Type
          | FunT Type Type
  deriving Eq

instance Show Type where
  show UnitT             = "()"
  show IntT              = "Int"
  show BoolT             = "Bool"
  show FloatT            = "Float"
  show SymbolT           = "Symbol"
  show (ListT t)         = "List (" ++ show t ++ ")"
  show (FunT p ret)    = show p ++ " -> " ++ show ret

data AST = Symbol Name
         | Int Int
         | Float Double
         | Bool Bool
         | Typed AST Type
         | List [AST]
         | Tree [AST]

instance Show AST where
  show (Symbol name) = name
  show (Int i)       = show i
  show (Float f)     = show f
  show (Bool True)   = "#t"
  show (Bool False)  = "#f"
  show (Typed a t)   = show a ++ ":" ++ show t
  show (List xs)   = "[" ++ unwords (map show xs) ++ "]"
  show (Tree xs) = "(" ++ unwords (map show xs) ++ ")"
  -- show (List [Symbol "quote", x]) = "'" ++ show x
  -- show (List (Symbol "quote":xs)) = "'" ++ show (List xs)
  -- show (List l)      = "(" ++ show_list l ++ ")"
  --   where show_list []     = ""
  --         show_list [x]    = show x
  --         show_list (x:xs) = show x ++ " " ++ show_list xs

sample1 = Tree [Symbol "def", Typed (Symbol "ans") IntT, Int 42]
sample2 = Typed (Tree [ Symbol "if"
                      , Tree [Symbol "==", Symbol "ans", Int 42]
                      , Tree [Symbol "quote", Symbol "yes"]
                      , Tree [Symbol "quote", Symbol "no"]]) SymbolT

sample3 = Typed (Tree [Symbol "def"
                      , Tree [Typed (Symbol "f") IntT, Typed (Symbol "x") IntT]
                      , Tree [Symbol "*", Symbol "x", Symbol "x"]]) SymbolT

sample4 = Typed (List [Symbol "a", Symbol "b"]) (ListT SymbolT)
