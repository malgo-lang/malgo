{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.Backend.MIR where

import           Language.Malgo.Prelude
import           Language.Malgo.Type

data Expr a = Var a
            | Int Integer
            | Float Double
            | Char Char
            | String Text
            | Unit
            | Tuple [a]
            | TupleAccess (Expr a) Int
            | Apply a [a]
            | Let a (Expr a) (Expr a)
            | If a (Expr a) (Expr a)
  deriving Show

data MType = IntTy { _bit :: Integer }
           | DoubleTy
           | PointerTy MType
           | StructTy [MType]
           | FunctionTy MType [MType]
  deriving (Show, Eq)

class HasMType a where
  mTypeOf :: a -> MType

instance HasMType MType where
  mTypeOf = identity

instance HasMType Type where
  mTypeOf (NameTy n) =
    case n of
      "Int"    -> IntTy 32
      "Float"  -> DoubleTy
      "Bool"   -> IntTy 1
      "Char"   -> IntTy 8
      "String" -> PointerTy (IntTy 8)
      "Unit"   -> StructTy []
      _        -> error $ show n ++ " is not valid type"
  mTypeOf (FunTy params ret) =
    FunctionTy (mTypeOf ret) (map mTypeOf params)
  mTypeOf (TupleTy xs) =
    PointerTy (StructTy (map mTypeOf xs))
  mTypeOf ClsTy{} = error "ClsTy is not have MType"

instance HasMType a => HasMType (Expr a) where
  mTypeOf (Var x) = mTypeOf x
  mTypeOf (Int _) = IntTy 32
  mTypeOf (Float _) = DoubleTy
  mTypeOf (Char _) = IntTy 8
  mTypeOf (String _) = PointerTy (IntTy 8)
  mTypeOf Unit = StructTy []
  mTypeOf (Tuple xs) = PointerTy (StructTy (map mTypeOf xs))
  mTypeOf (TupleAccess xs i) =
    case mTypeOf xs of
      (PointerTy (StructTy xs')) ->
        fromMaybe (error "out of bounds") (atMay xs' i)
      _ -> error "invalied MType"
  mTypeOf (Apply f _) =
    case mTypeOf f of
      FunctionTy x _ -> x
      _              -> error "invalied MType"
  mTypeOf (Let _ _ e) = mTypeOf e
  mTypeOf (If _ e _) = mTypeOf e
