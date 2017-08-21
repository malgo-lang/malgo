{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.Syntax where

import           Data.List       (intercalate)
import           Text.Parsec.Pos (SourcePos, newPos)

type Name = String

data Obj = Symbol Name
         | Int Integer
         | Float Double
         | Bool Bool
         | Char Char
         | String String
         -- | Typed Obj Obj
         | List [XObj]
         | Tree [XObj]
  deriving (Eq, Show)

type Info = SourcePos
dummyInfo :: Info
dummyInfo = newPos "" 0 0

data Ty = SymbolTy
        | IntTy
        | FloatTy
        | BoolTy
        | CharTy
        | StringTy
        | AtomTy String
        | ListTy Ty
        | FuncTy [Ty] Ty
  deriving (Eq, Show)

data XObj = XObj { obj  ::  Obj
                 , info :: Maybe Info
                 , ty   :: Maybe Ty}
  deriving (Eq, Show)

-- pretty :: Obj -> String
-- pretty (Symbol name) = name
-- pretty (Int i)       = show i
-- pretty (Float f)     = show f
-- pretty (Bool True)   = "#t"
-- pretty (Bool False)  = "#f"
-- pretty (Char c)      = show c
-- pretty (String s)    = show s
-- pretty (Typed a t)   = pretty a ++ ":" ++ pretty t
-- pretty (List xs)     = "[" ++ unwords (map pretty xs) ++ "]"
-- pretty (Tree [Symbol "quote", Symbol s]) = "'" ++ s
-- pretty (Tree xs)     = "(" ++ unwords (map pretty xs) ++ ")"
