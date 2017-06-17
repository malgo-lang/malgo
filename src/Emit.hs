{-# LANGUAGE OverloadedStrings #-}

module Emit where

import           LLVM.AST
import qualified LLVM.AST              as AST
import           LLVM.AST.Global
import           LLVM.Context
import           LLVM.Module
import qualified Syntax                as S

import           Control.Monad.Except
import           Data.ByteString.Char8 as BS
import           Data.ByteString.Short as BS (toShort)

transType :: S.Type -> Type
transType "int" = IntegerType 32
transType _     = error "Illigal type"

transName :: S.Name -> Name
transName name = Name $ BS.toShort (BS.pack name)

transParams :: [(S.Name, S.Type)] -> [Parameter]
transParams [] = []
transParams ((name, ty):xs) =
  (Parameter (transType ty) (transName name) []):(transParams xs)

transBody :: [S.Expr] -> BasicBlock
transBody _ = undefined

transDefn :: S.Expr -> Definition
transDefn S.Defn { S.name = name, S.returnType = returnType,
                   S.params = params, S.body = body} =
  GlobalDefinition functionDefaults
  { name = transName name
  , parameters = ( transParams params, False )
  , returnType = transType returnType
  , basicBlocks = [transBody body]
  }
