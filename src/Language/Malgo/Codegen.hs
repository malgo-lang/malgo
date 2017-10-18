{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Language.Malgo.Codegen where

-- llvm-hs/llvm-hs-kaleidoscopeベース
import           Control.Monad.State
import qualified Data.ByteString       as B
import qualified Data.ByteString.Short as BS
import           Data.Char             (chr, ord)
import           Data.String           (IsString, fromString)
import           Language.Malgo.HIR    (Id (..))
import qualified Language.Malgo.MIR    as MIR
import           Language.Malgo.Syntax (Type (..))
import qualified LLVM.AST              as AST
import qualified LLVM.AST.Constant     as Constant
import qualified LLVM.AST.Float        as Float
import qualified LLVM.AST.Global       as Global
import qualified LLVM.AST.Name         as Name
import qualified LLVM.AST.Type         as Type

makeModule :: String -> [AST.Definition] -> AST.Module
makeModule name definitions =
  AST.defaultModule { AST.moduleName = fromString name
                    , AST.moduleDefinitions = definitions
                    }

fromId :: IsString a => Id -> a
fromId (Sym s) = fromString s

str2Name = Name.Name . fromString

id2Name = Name.Name . fromId

-- Type
compileType :: Type -> Type.Type
compileType IntTy    = Type.i32
compileType FloatTy  = Type.double
compileType BoolTy   = Type.i1
compileType CharTy   = Type.i8
compileType StringTy = Type.ptr Type.i8
compileType UnitTy   = Type.ptr Type.i1
compileType (FunTy retTy argTys) = Type.FunctionType (compileType retTy) (map compileType argTys) False

-- DECL
compileDECL :: MIR.DECL -> AST.Definition
compileDECL (MIR.DEF name typ expr) =
  AST.GlobalDefinition AST.globalVariableDefaults
  { Global.name = fromId name
  , Global.type' = compileType typ
  , Global.initializer = Just (compileConstant expr)
  }
compileDECL (MIR.DEFUN fn retTy params body) =
  AST.GlobalDefinition AST.functionDefaults
  { Global.name = fromId fn
  , Global.returnType = compileType retTy
  , Global.parameters = ( [Global.Parameter
                           (compileType ty)
                           (id2Name n) [] | (n, ty) <- params]
                        , False )
  , Global.basicBlocks = [compileBLOCK body]
  }

-- EXPR

compileConstant :: MIR.EXPR -> Constant.Constant
compileConstant (MIR.INT x, _)      = Constant.Int 32 (toInteger x)
compileConstant (MIR.FLOAT x, _)    = Constant.Float (Float.Double x)
compileConstant (MIR.BOOL True, _)  = Constant.Int 1 1
compileConstant (MIR.BOOL False, _) = Constant.Int 1 0
compileConstant (MIR.CHAR x, _)     = Constant.Int 8 (toInteger (ord x))
compileConstant (MIR.STRING x, _) =
  Constant.Array (compileType StringTy) (map (Constant.Int 8 . toInteger . ord) x)
compileConstant (MIR.UNIT, _) = Constant.GlobalReference (compileType UnitTy) (str2Name "unit")
compileConstant (MIR.VAR x, ty) = Constant.GlobalReference (compileType ty) (id2Name x)
compileConstant e = error $ "MIR -> LLVM error: " ++ show e ++ " is not Constant"

-- BLOCK
compileBLOCK = undefined
