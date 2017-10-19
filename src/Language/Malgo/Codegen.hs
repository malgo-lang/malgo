{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.Codegen where

import           Control.Monad.State
import           Data.Char
import           Data.String
import           Language.Malgo.LLVM
import qualified Language.Malgo.LLVM   as L
import           Language.Malgo.MIR
import           Language.Malgo.Syntax
import qualified LLVM.AST.Constant     as C
import qualified LLVM.AST.Float        as F
import qualified LLVM.AST.Name         as Name
import qualified LLVM.AST.Type         as T

compileType IntTy = L.intTy
compileType FloatTy = L.floatTy
compileType BoolTy = L.boolTy
compileType CharTy = L.charTy
compileType UnitTy = L.unitTy
compileType StringTy = L.stringTy
compileType (FunTy retTy argTys) =
  L.funTy (compileType retTy) (map compileType argTys)

compileConst :: EXPR -> C.Constant
compileConst (INT x, _)      = C.Int 32 (toInteger x)
compileConst (FLOAT x, _)    = C.Float (F.Double x)
compileConst (CHAR x, _)     = C.Int 8 (toInteger (ord x))
compileConst (BOOL True, _)  = C.Int 1 1
compileConst (BOOL False, _) = C.Int 1 0
compileConst (STRING x, _)   = C.Array L.charTy (map (C.Int 8 . toInteger . ord) x)
compileConst (UNIT, _) = C.GlobalReference L.unitTy (Name.Name "unit")
compileConst x = error $ "error: " ++ show x ++ " is not a Const"

compileDECL :: DECL -> L.LLVM ()
compileDECL (DEF name StringTy (STRING x, _)) =
  L.defineVar (L.fromId name) (T.ArrayType (fromInteger (toInteger (length x + 1)))
                              L.charTy)
  (compileConst (STRING x, StringTy))
compileDECL (DEF name ty val) =
  L.defineVar (L.fromId name) (compileType ty) (compileConst val)
compileDECL (DEFUN fn retTy params body) =
  L.defineFunc
  (compileType retTy)
  (L.fromId fn)
  [(compileType ty, L.fromId nm) | (nm, ty) <- params]
  (compileBody body)
compileDECL (EXDEF name ty) =
  L.externalVar (L.fromId name) (compileType ty)
compileDECL (EXDEFUN fn retTy params) =
  L.externalFunc
  (compileType retTy)
  (L.fromId fn)
  [(compileType ty, L.fromId nm) | (nm, ty) <- params]

compileBody (BLOCK _ xs) = mapM compileStm xs

-- compileStm :: EXPR -> Codegen a
compileStm (LET name typ val, _) = do
  var <- alloca (compileType typ) (Just (id2Name name))
  val' <- compileOperand val
  store var val'
  assign (fromId name) var
compileStm (RET name ty, _) = do
  var <- getvar (fromId name)
  ret var
compileStm ()

compileOperand = undefined
