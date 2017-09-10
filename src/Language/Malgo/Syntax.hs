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

class Symantics repr where
  symbol :: Name -> repr
  int :: Integer -> repr
  float :: Double -> repr
  bool :: Bool -> repr
  char :: Char -> repr
  string :: String -> repr
  list :: [repr] -> repr
  (-:) :: repr -> repr -> repr

instance Symantics AST where
  symbol n = Symbol n
  int i = Int i
  float f = Float f
  bool b = Bool b
  char c = Char c
  string s = String s
  list xs = List xs
  e -: t = e :-: t

instance Symantics String where
  symbol n = n
  int i = show i
  float f = show f
  bool b = if b then "#t" else "#f"
  char c = show c
  string s = show s
  list xs = "(" ++ unwords xs ++ ")"
  e -: t = e ++ ":" ++ t

astToSym :: Symantics a => AST -> a
astToSym (Symbol x) = symbol x
astToSym (Int x)    = int x
astToSym (Float x)  = float x
astToSym (Bool x)   = bool x
astToSym (Char x)   = char x
astToSym (String x) = string x
astToSym (List xs)  = list (map astToSym xs)
astToSym (e :-: t)  = (astToSym e) -: (astToSym t)
