{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.CodeGen where

import           Data.String
import           Data.Text.Lazy.IO          as T

import qualified LLVM.AST.AddrSpace         as AS
import qualified LLVM.AST.Constant          as C
import qualified LLVM.AST.IntegerPredicate  as IP
import           LLVM.AST.Operand
import           LLVM.AST.Type              as LT
import           LLVM.IRBuilder.Constant
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad
import           LLVM.Pretty

import           Language.Malgo.Rename      (ID (..))
import qualified Language.Malgo.Type        as T
import           Language.Malgo.TypeCheck   (TypedID (..))
import           Language.Malgo.Utils
-- test = T.putStrLn $ ppllvm $ buildModule "test" $ mdo
--   f <- function "fun" [(i32, "n")] i32 $ \[n] ->
--     do r <- call f [(n, [])]
--        ret r
--   return f

convertType :: T.Type -> Type
convertType "Int"          = LT.i32
convertType "Float"        = LT.double
convertType "Bool"         = LT.i1
convertType "Char"         = LT.i8
convertType "String"       = LT.ptr LT.i8
convertType "Unit"         = NamedTypeReference "Unit"
convertType (T.NameTy x) = error $ "unknown type: " ++ show x
convertType (T.TupleTy xs) =
  StructureType False (map convertType xs)
convertType (T.FunTy params retTy) =
  FunctionType (convertType retTy) (map convertType params) False
convertType (T.ClsTy _ _) = error "closure is not supported"

toGrobalRef :: TypedID -> Operand
toGrobalRef x =
  ConstantOperand (C.GlobalReference (toType x) (fromTypedID x))
  where
    toType (TypedID _ ty) = convertType ty

fromTypedID :: IsString a => TypedID -> a
fromTypedID (TypedID i _) =
  fromString $ show (_name i) ++ "zi" ++ show (_uniq i)
