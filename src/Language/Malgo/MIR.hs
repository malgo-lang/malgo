{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.MIR
  ( Decl(..)
  , Expr(..)
  , Instr(..)
  , Program(..)
  , typeOf
  ) where

import           Language.Malgo.HIR       (Op (..))
import           Language.Malgo.Type
import           Language.Malgo.TypeCheck (TypedID (..))
import           Language.Malgo.Utils

data Expr a = Var a
            | Int Integer
            | Float Double
            | Bool Bool
            | Char Char
            | Unit
            | MakeCls a [a]
            | CallDir { _fn   :: a
                      , _args :: [a]
                      }
            | CallCls { _cls  :: a
                      , _args :: [a]
                      }
            | If a (Expr a) (Expr a)
            | BinOp Op a a
            deriving (Show)

typeOf (Var (TypedID _ ty)) = ty
typeOf (Int _) = "Int"
typeOf (Float _) = "Float"
typeOf (Bool _) = "Bool"
typeOf (Char _) = "Char"
typeOf Unit = "Unit"
typeOf (MakeCls (TypedID _ ty) _) = ty
typeOf (CallDir (TypedID _ (FunTy _ ty)) _) = ty
typeOf (CallCls (TypedID _ (ClsTy _ ty _)) _) = ty
typeOf (If _ t _) = typeOf t
typeOf (BinOp op _ _) =
  case op of
   Add     -> "Int"
   Sub     -> "Int"
   Mul     -> "Int"
   Div     -> "Int"
   FAdd    -> "Float"
   FSub    -> "Float"
   FMul    -> "Float"
   FDiv    -> "Float"
   Mod     -> "Float"
   (Eq _)  -> "Bool"
   (Neq _) -> "Bool"
   (Lt _)  -> "Bool"
   (Gt _)  -> "Bool"
   (Le _)  -> "Bool"
   (Ge _)  -> "Bool"
   And     -> "Bool"
   Or      -> "Bool"

data Instr a = a := (Expr a)
             | Do (Expr a)
             deriving Show

data Decl a = FunDec { _decName :: a
                     , _params  :: [a]
                     , _capture :: [a]
                     , _body    :: [Instr a]
                     }
            | StrDec { _decName :: a
                     , _val     :: String
                     }
            | ExDec { _decName :: a
                    , _actual  :: String
                    }
            deriving Show

data Program a = Program { _toplevel :: [Decl a]
                         , _main     :: [Instr a]
                         }
  deriving Show
