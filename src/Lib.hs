{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( printadd
    ) where

import           LLVM.AST
import qualified LLVM.AST              as AST
import           LLVM.AST.Global
import           LLVM.Context
import           LLVM.Module

import           Control.Monad.Except
import           Data.ByteString.Char8 as BS

int :: Type
int = IntegerType 32

defAdd :: Definition
defAdd = GlobalDefinition functionDefaults
  { name = Name "add"
  , parameters =
      ( [ Parameter int (Name "a") []
        , Parameter int (Name "b") [] ]
      , False )
  , returnType = int
  , basicBlocks = [body]
  }
  where
    body = BasicBlock
      (Name "entry")
      [ Name "result" := Add False False (LocalReference int (Name "a")) (LocalReference int (Name "b")) []]
      (Do $ Ret (Just (LocalReference int (Name "result"))) [])

module_ :: AST.Module
module_ = defaultModule
  { moduleName = "basic"
  , moduleDefinitions = [defAdd]
  }

toLLVM :: AST.Module -> IO ()
toLLVM mod = withContext $ \ctx -> do
  llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
  BS.putStrLn llvm

printadd :: IO ()
printadd = toLLVM module_
