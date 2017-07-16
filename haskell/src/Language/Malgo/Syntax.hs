{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.Syntax where

import           Control.Monad.State (evalState)

type Name = String
data Type = UnitT
          | IntT
          | BoolT
          | FloatT
          | SymbolT
  deriving Eq

instance Show Type where
  show UnitT   = "()"
  show IntT    = "Int"
  show BoolT   = "Bool"
  show FloatT  = "Float"
  show SymbolT = "Symbol"

data AST = Symbol Name
         | Int Int
         | Float Double
         | Bool Bool
         | Typed AST Type
         | List [AST]

instance Show AST where
  show (Symbol name) = name
  show (Int i)       = show i
  show (Float f)     = show f
  show (Bool True)   = "#t"
  show (Bool False)  = "#f"
  show (Typed a t)   = show a ++ ":" ++ show t
  show (List ((Symbol "quote"):x:[])) = "'" ++ show x
  show (List ((Symbol "quote"):xs)) = "'" ++ show (List xs)
  show (List l)      = "(" ++ show_list l ++ ")"
    where show_list []     = ""
          show_list [x]    = show x
          show_list (x:xs) = show x ++ " " ++ show_list xs

sample1 = List [Symbol "def", Typed (Symbol "ans") IntT, Int 42]
sample2 = Typed (List [ Symbol "if"
                      , List [Symbol "==", Symbol "ans", Int 42]
                      , List [Symbol "quote", Symbol "yes"]
                      , List [Symbol "quote", Symbol "no"]]) SymbolT
