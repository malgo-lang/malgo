{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Language.Malgo.TypeRep.MType where

import           Control.Monad.Except  (MonadError, throwError)
import           Language.Malgo.ID
import           Language.Malgo.Pretty
import           Relude

data MType = IntTy Integer
           | DoubleTy
           | PointerTy MType
           | StructTy [MType]
           | FunctionTy MType [MType]
  deriving (Show, Eq, Read, Ord, Generic, PrettyVal)

class HasMType a where
  mTypeOf :: a -> MType

instance HasMType MType where
  mTypeOf = id

instance HasMType a => HasMType (ID a) where
  mTypeOf (ID _ _ m) = mTypeOf m

instance Pretty MType where
  pPrint (IntTy i) = "i" <> pPrint i
  pPrint DoubleTy = "double"
  pPrint (PointerTy t) = parens $ "ptr" <+> pPrint t
  pPrint (StructTy ts) = parens $ "struct" <+> parens (sep (punctuate "," $ map pPrint ts))
  pPrint (FunctionTy ret params) =
    parens $ "fun" <+> pPrint ret
    <+> parens (sep (punctuate "," $ map pPrint params))

accessMType :: MonadError Doc m => MType -> [Int] -> m MType
accessMType x [] = return x
accessMType (PointerTy x) (_:is) = accessMType x is
accessMType t@(StructTy xs) (i:is) =
  case atMay xs i of
    Just xt -> accessMType xt is
    Nothing -> throwError $ "out of bounds:" <+> (pPrint t <> ",") <+> pPrint i
  where atMay (y:_) 0  = Just y
        atMay [] _     = Nothing
        atMay (_:ys) n = atMay ys (n - 1)
accessMType t _ = throwError $ pPrint t <+> "is not accessable MType"
