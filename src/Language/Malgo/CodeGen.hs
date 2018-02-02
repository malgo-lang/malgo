{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.CodeGen where

import           Control.Monad.State.Strict
import           Data.Char
import qualified Data.Map.Strict            as Map
import           Data.String
import           Data.Text.Lazy.IO          as T

import qualified LLVM.AST.AddrSpace         as AS
import qualified LLVM.AST.Constant          as C
import qualified LLVM.AST.Global            as G
import qualified LLVM.AST.IntegerPredicate  as IP
import           LLVM.AST.Operand
import qualified LLVM.AST.Type              as LT
import           LLVM.IRBuilder.Constant
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad
import           LLVM.Pretty

import           Language.Malgo.MIR
import           Language.Malgo.Rename      (ID (..))
import qualified Language.Malgo.Type        as T
import           Language.Malgo.TypeCheck   (TypedID (..))
import           Language.Malgo.Utils

-- test = T.putStrLn $ ppllvm $ buildModule "test" $ mdo
--   f <- function "fun" [(i32, "n")] i32 $ \[n] ->
--     do r <- call f [(n, [])]
--        ret r
--   return f

type CodeGen a = IRBuilderT (State (Map.Map TypedID Operand)) a

dumpCodeGen :: CodeGen a -> [G.BasicBlock]
dumpCodeGen m = flip evalState Map.empty $ execIRBuilderT emptyIRBuilder m

convertType :: T.Type -> LT.Type
convertType "Int"          = LT.i32
convertType "Float"        = LT.double
convertType "Bool"         = LT.i1
convertType "Char"         = LT.i8
convertType "String"       = LT.ptr LT.i8
convertType "Unit"         = LT.NamedTypeReference "Unit"
convertType (T.NameTy x) = error $ "unknown type: " ++ show x
convertType (T.TupleTy xs) =
  LT.StructureType False (map convertType xs)
convertType (T.FunTy params retTy) =
  LT.FunctionType (convertType retTy) (map convertType params) False
convertType (T.ClsTy _ _) = error "closure is not supported"
-- TODO: クロージャをLLVMでどのように扱うかを決める

getRef :: TypedID -> CodeGen Operand
getRef i = do
  m <- lift get
  case Map.lookup i m of
    Nothing -> error $ show i ++ " is not found in " ++ show m
    Just x  -> return x

fromTypedID :: IsString a => TypedID -> a
fromTypedID (TypedID i _) =
  fromString $ show (_name i) ++ "zi" ++ show (_uniq i)


genExpr :: Expr TypedID -> CodeGen ()
genExpr (Var a)   = ret =<< getRef a
genExpr (Int i)   = ret =<< int32 i
genExpr (Float d) = ret =<< double d
genExpr (Bool b)  = ret =<< bit (if b then 1 else 0)
genExpr (Char c)  = ret =<< char (toInteger . ord $ c)
  where char = pure . ConstantOperand . C.Int 8
genExpr (String _) = error "string is not supported"
  -- TODO: GCを使えるようにする。ラッパーを書く
