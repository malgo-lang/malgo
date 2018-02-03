{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Language.Malgo.CodeGen where

import           Control.Monad.State.Strict
import           Data.Char
import qualified Data.Map.Strict            as Map
import           Data.Maybe
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

data CodeGenState = CodeGenState { _table    :: Map.Map TypedID Operand
                                 , _term     :: Operand -> CodeGen () -- if式の際の最終分岐先などに利用
                                 , _internal :: Map.Map String Operand
                                 }
type CodeGen a = IRBuilderT (State CodeGenState) a

addTable :: TypedID -> Operand -> CodeGen ()
addTable name opr =
  lift (modify (\s -> s { _table = Map.insert name opr (_table s)}))

addInternal name opr =
  lift (modify (\s -> s { _internal = Map.insert name opr (_internal s) }))

dumpCodeGen :: CodeGen a -> [G.BasicBlock]
dumpCodeGen m = flip evalState (CodeGenState Map.empty ret Map.empty) $ execIRBuilderT emptyIRBuilder
  (do addInternal "GC_malloc" (ConstantOperand
                               (C.GlobalReference
                                 (LT.FunctionType (LT.ptr LT.i8) [LT.i64] False)
                                "GC_malloc"))
      m)

convertType :: T.Type -> LT.Type
convertType "Int"          = LT.i32
convertType "Float"        = LT.double
convertType "Bool"         = LT.i1
convertType "Char"         = LT.i8
convertType "String"       = LT.ptr LT.i8
convertType "Unit"         = -- LT.NamedTypeReference "Unit"
  LT.StructureType False []
convertType (T.NameTy x) = error $ "unknown type: " ++ show x
convertType (T.TupleTy xs) =
  LT.StructureType False (map convertType xs)
convertType (T.FunTy params retTy) =
  LT.FunctionType (convertType retTy) (map convertType params) False
convertType (T.ClsTy _ _) = error "closure is not supported"
-- TODO: クロージャをLLVMでどのように扱うかを決める

getRef :: TypedID -> CodeGen Operand
getRef i = do
  m <- lift (gets _table)
  case Map.lookup i m of
    Nothing -> error $ show i ++ " is not found in " ++ show m
    Just x  -> return x

term :: CodeGen Operand -> CodeGen ()
term o = do
  t <- lift (gets _term)
  o' <- o
  t o'

fromTypedID :: IsString a => TypedID -> a
fromTypedID (TypedID i _) =
  fromString $ show (_name i) ++ "zi" ++ show (_uniq i)

char = pure . ConstantOperand . C.Int 8

gcMalloc :: Integer -> CodeGen Operand
gcMalloc bytes = do
  f <- lift (fromJust . Map.lookup "GC_malloc" <$> gets _internal)
  bytes' <- int64 bytes
  call f [(bytes', [])]

genExpr :: Expr TypedID -> CodeGen ()
genExpr e@(Var _)   = term (genExpr' e) `named` "var"
genExpr e@(Int _)   = term (genExpr' e) `named` "int"
genExpr e@(Float _) = term (genExpr' e) `named` "float"
genExpr e@(Bool _)  = term (genExpr' e) `named` "bool"
genExpr e@(Char _)  = term (genExpr' e) `named` "char"
genExpr e@(String _) = term (genExpr' e) `named` "string"
genExpr e@Unit = term (genExpr' e) `named` "unit"
genExpr (Tuple _) = error "tuple is not supported"
genExpr (TupleAccess _ _) = error "tuple is not supported"
genExpr (CallCls _ _) = error "closure is not supported"
genExpr e@(CallDir _ _) = term (genExpr' e) `named` "calldir"
genExpr e@(Let (ValDec _ _) _) = do
  term (genExpr' e) `named` "let"
genExpr (Let (ClsDec _ _ _) _) = error "closure is not supported"
genExpr e@(If _ _ _) = term (genExpr' e) `named` "if"

genExpr' :: Expr TypedID -> CodeGen Operand
genExpr' (Var a)    = getRef a `named` "var"
genExpr' (Int i)    = int32 i `named` "int"
genExpr' (Float d)  = double d `named` "float"
genExpr' (Bool b)   = bit (if b then 1 else 0) `named` "bool"
genExpr' (Char c)   = char (toInteger . ord $ c) `named` "char"
genExpr' (String xs) = do
  p <- gcMalloc (toInteger $ length xs + 1) `named` "string"
  mapM_ (uncurry $ addChar p) (zip [0..] $ xs ++ ['\0'])
  return p
  where addChar p i c = do
          i' <- int32 i
          p' <- gep p [i'] `named` "tmp_char"
          c' <- char (toInteger . ord $ c)
          store p' 1 c'
genExpr' Unit       = (pure . ConstantOperand . C.Undef $ convertType "Unit") `named` "unit"
genExpr' (CallDir fn args) = do
  fn' <- getRef fn
  args' <- mapM (\a -> do a' <- getRef a; return (a', [])) args
  call fn' args'
genExpr' (Let (ValDec name val) e) = do
  val' <- genExpr' val `named` (fromString . show $ pretty name)
  addTable name val'
  genExpr' e
genExpr' (If c t f) = do
  c' <- getRef c
  r <- alloca (convertType (T.typeOf t)) Nothing 0 `named` "resultptr"
  (end, t', f') <- (,,) <$> freshName "end" <*> freshName "then" <*> freshName "else"
  condBr  c' t' f'
  backup <- lift (gets _term)
  lift (modify $ \s -> s { _term = \o -> store r 0 o >> br end })
  emitBlockStart t'; genExpr t
  emitBlockStart f'; genExpr f
  lift (modify $ \s -> s { _term = backup })
  emitBlockStart end;
  load r 0

-- TODO: GCを使えるようにする。ラッパーを書く
--       Unitの表現を決める
