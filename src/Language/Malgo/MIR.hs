{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.MIR where

import           Language.Malgo.HIR       (Op (..))
import           Language.Malgo.Type
import           Language.Malgo.TypeCheck (TypedID (..))
import           Language.Malgo.Utils

data Expr a = Var a
            | Int Integer
            | Float Double
            | Bool Bool
            | Char Char
            | String String
            | Unit
            | CallDir { _fn   :: a
                      , _args :: [a]
                      }
            | CallCls { _cls  :: a
                      , _args :: [a]
                      }
            | Let (Decl a) (Expr a)
            | If a (Expr a) (Expr a)
            | BinOp Op a a
            deriving (Show)

instance Typeable a => Typeable (Expr a) where
  typeOf (Var name) = typeOf name
  typeOf (Int _)    = "Int"
  typeOf (Float _)  = "Float"
  typeOf (Bool _)   = "Bool"
  typeOf (Char _)   = "Char"
  typeOf Unit       = "Unit"
  typeOf (CallDir name _) =
    case typeOf name of
      (FunTy _ ty) -> ty
  typeOf (CallCls name _) =
    case typeOf name of
      (ClsTy _ ty _) -> ty
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

data FunDec a = FunDec { _name    :: a
                       , _params  :: [a]
                       , _capture :: [a]
                       , _body    :: Expr a
                       }
  deriving Show

data Decl a = ExDec { _decName :: a
                    , _actual  :: String
                    }
            | ValDec { _decName :: a
                     , _value   :: Expr a
                     }
            | ClsDec { _decName :: a
                     , _fnName  :: a
                     , _fv      :: [a]
                     }
            deriving Show

data Program a = Program { _toplevel :: [FunDec a]
                         , _main     :: Expr a
                         }
  deriving Show
