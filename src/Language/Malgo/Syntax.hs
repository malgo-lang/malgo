{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Malgo.Syntax where

type Name = String

data AST = Symbol Name
         | Int Integer
         | Float Double
         | Bool Bool
         | Char Char
         | String String
         | List [AST]
         | AST :-: AST
         deriving (Eq, Show)

pretty :: AST -> String
pretty (Symbol n)  = n
pretty (Int i)     = show i
pretty (Float f)   = show f
pretty (Bool b)    = if b then "#t" else "#f"
pretty  (Char c)   = show c
pretty  (String s) = show s
pretty  (List xs)  = "(" ++ unwords (map pretty xs) ++ ")"
pretty  (e :-: t)  = pretty e ++ ":" ++ pretty t
