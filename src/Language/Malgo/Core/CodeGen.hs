{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Malgo.Core.CodeGen where

import Control.Monad.Cont
import Data.Char (ord)
import qualified Data.Map as Map
import qualified Data.Text as T
import GHC.Exts (fromList)
import qualified LLVM.AST
import LLVM.AST.Constant (Constant (..))
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate as IP
import LLVM.AST.Operand (Operand (..))
import LLVM.AST.Type hiding (double, void)
import qualified LLVM.AST.Type as LT
import LLVM.IRBuilder
import qualified LLVM.IRBuilder as IRBuilder
import Language.Malgo.IR.Core as Core
import Language.Malgo.Id
import Language.Malgo.Monad
import Language.Malgo.Pass
import Language.Malgo.Prelude hiding (from, index, op, to)
import Language.Malgo.Pretty
import Language.Malgo.TypeRep.CType as CType

data CodeGen

instance Pass CodeGen (Program (Id CType)) [LLVM.AST.Definition] where
  passName = "GenLLVM"
  isDump _ = False
  trans Program {topBinds, mainExp, topFuncs} = execModuleBuilderT emptyModuleBuilder $ undefined

type OprMap = IdMap CType Operand

type PrimMap = Map Text Operand

convType :: CType -> Type
convType (ps :-> r) = ptr $ StructureType False [ptr i8, ptr $ FunctionType (convType r) (ptr i8 : map convType ps) False]
convType IntT = i64
convType FloatT = LT.double
convType CharT = i8
convType StringT = ptr i8
convType (PackT _) = ptr (StructureType False [i64, ptr i64])
convType (ArrayT ty) = ptr $ convType ty
convType AnyT = ptr i64

findVar :: MonadReader OprMap m => Id CType -> m Operand
findVar x = do
  env <- ask
  case env ^. at x of
    Just x' -> pure x'
    Nothing -> error $ show $ pPrint x <> " is not found"

findExt :: (MonadState PrimMap m, MonadModuleBuilder m) => Text -> [Type] -> Type -> m Operand
findExt x ps r = do
  env <- get
  case env ^. at x of
    Just x' -> pure x'
    Nothing -> do
      opr <- extern (LLVM.AST.mkName $ T.unpack x) ps r
      modify (at x ?~ opr)
      pure opr

mallocBytes ::
  ( MonadState PrimMap m,
    MonadModuleBuilder m,
    MonadIRBuilder m
  ) =>
  Operand ->
  Maybe Type ->
  m Operand
mallocBytes bytesOpr maybeType = do
  gcMalloc <- findExt "GC_malloc" [i64] (ptr i8)
  ptrOpr <- call gcMalloc [(bytesOpr, [])]
  case maybeType of
    Just t -> bitcast ptrOpr t
    Nothing -> pure ptrOpr

toName :: Id a -> LLVM.AST.Name
toName x = LLVM.AST.mkName $ x ^. idName <> show (x ^. idUniq)

genFunc ::
  ( MonadModuleBuilder m,
    MonadReader OprMap m,
    MonadState PrimMap m,
    MonadUniq m
  ) =>
  Id CType ->
  [Id CType] ->
  Exp (Id CType) ->
  m Operand
genFunc name params body = function funcName llvmParams retty $ \args ->
  local (fromList (zip params args) <>) $ genExp body ret
  where
    funcName = toName name
    llvmParams = map (\x -> (convType $ x ^. idMeta, ParameterName $ fromString $ x ^. idName)) params
    retty = convType (cTypeOf body)

genExp (Atom x) k = k =<< genAtom x
genExp (Call f xs) k = do
  fOpr <- genAtom f
  xsOprs <- traverse genAtom xs
  captureOpr <- (load ?? 0) =<< gep fOpr [int32 0, int32 0]
  funcOpr <- (load ?? 0) =<< gep fOpr [int32 0, int32 1]
  k =<< call funcOpr (map (,[]) $ captureOpr : xsOprs)
genExp (CallDirect f xs) k = do
  fOpr <- findVar f
  xsOprs <- traverse genAtom xs
  k =<< call fOpr (map (,[]) xsOprs)
genExp (PrimCall name (ps :-> r) xs) k = do
  primOpr <- findExt name (map convType ps) (convType r)
  xsOprs <- traverse genAtom xs
  k =<< call primOpr (map (,[]) xsOprs)
genExp (PrimCall _ t _) _ = error $ show $ pPrint t <> " is not fuction type"
genExp (ArrayRead a i) k = do
  aOpr <- genAtom a
  iOpr <- genAtom i
  k =<< (load ?? 0) =<< gep aOpr [iOpr]
genExp (ArrayWrite a i v) k = do
  aOpr <- genAtom a
  iOpr <- genAtom i
  vOpr <- genAtom v
  addr <- gep aOpr [iOpr]
  store addr 0 vOpr
  k (ConstantOperand (Undef (ptr i64)))
genExp (Let xs e) k = do
  env <- fromList <$> traverse prepare xs
  local (env <>) $ do
    traverse_ loadDef xs
    genExp e k
  where
    prepare (name, _) = do
      opr <- mallocBytes (ConstantOperand $ C.sizeof $ convType $ cTypeOf name) (Just $ convType $ cTypeOf name)
      pure (name, opr)
    loadDef (name, obj) = do
      addr <- findVar name
      genObj addr obj
genExp (Match e cs) k =
  case cTypeOf e of
    PackT union -> _
    _ -> genBind e cs k

genBind v (Bind x e :| _) k =
  genExp v $ \vOpr ->
    local (at x ?~ vOpr) $ genExp e k
genBind _ (Unpack {} :| _) _ = bug Unreachable

genAtom ::
  ( MonadReader OprMap (t m),
    MonadTrans t,
    MonadUniq m,
    MonadModuleBuilder (t m)
  ) =>
  Atom (Id CType) ->
  t m Operand
genAtom (Var x) = findVar x
genAtom (Unboxed (Core.Int x)) = pure $ int64 x
genAtom (Unboxed (Core.Float x)) = pure $ double x
genAtom (Unboxed (Core.Char x)) = pure $ int8 $ toInteger $ ord x
genAtom (Unboxed (Core.String x)) = do
  i <- lift getUniq
  ConstantOperand <$> globalStringPtr x (LLVM.AST.mkName $ "str" <> show i)

genObj addr obj = _
