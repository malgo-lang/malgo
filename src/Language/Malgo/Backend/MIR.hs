{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ViewPatterns              #-}
module Language.Malgo.Backend.MIR where

import           Language.Malgo.Prelude
import           Language.Malgo.Type
import           Text.PrettyPrint       (Doc)

data Defn a = DefFun { _fnName   :: a
                     , _fnParams :: [a]
                     , _fnBody   :: Expr a
                     }
            | DefEx { _exName :: a }
  deriving (Show, Generic)

data Expr a = Var a
            | Int Integer
            | Float Double
            | Char Char
            | String Text
            | Unit
            | Tuple [a]
            | Apply a [a]
            | Let a (Expr a) (Expr a)
            | Cast MType a
            | Access (Expr a) [Int]
            | If a (Expr a) (Expr a)
  deriving (Show, Generic)

data MType = IntTy { _bit :: Integer }
           | DoubleTy
           | PointerTy MType
           | StructTy [MType]
           | FunctionTy MType [MType]
  deriving (Show, Eq, Generic)

accessMType :: MType -> [Int] -> Maybe MType
accessMType x [] = Just x
accessMType (PointerTy x) (_:is) = accessMType x is
accessMType (StructTy xs) (i:is) = do
  xt <- atMay xs i
  accessMType xt is
accessMType _ _ = Nothing

toMType :: (Typeable a, Outputable a) => a -> Either Doc MType
toMType (typeOf -> NameTy n) =
  case n of
    "Int"    -> Right $ IntTy 32
    "Float"  -> Right DoubleTy
    "Bool"   -> Right $ IntTy 1
    "Char"   -> Right $ IntTy 8
    "String" -> Right $ PointerTy (IntTy 8)
    "Unit"   -> Right $ StructTy []
    _        -> Left $ ppr n <> " is not valid type"
toMType (typeOf -> FunTy params ret) =
  FunctionTy <$> toMType ret <*> mapM toMType params
toMType (typeOf -> TupleTy xs) =
  PointerTy . StructTy <$> mapM toMType xs
toMType (typeOf -> ClsTy{}) =
  Left "ClsTy does not have MType"
toMType x = Left $ "unreachable: " <> ppr x
