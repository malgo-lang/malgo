{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Language.Malgo.Core.CodeGen where

import Control.Monad.Cont
import Data.Char (ord)
import Data.Map ()
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Exts (fromList)
import qualified LLVM.AST
import LLVM.AST.Constant (Constant (..))
import qualified LLVM.AST.Constant as C
import LLVM.AST.FloatingPointPredicate ()
import LLVM.AST.IntegerPredicate ()
import LLVM.AST.Operand (Operand (..))
import LLVM.AST.Type hiding (double, void)
import qualified LLVM.AST.Type as LT
import LLVM.AST.Typed ()
import LLVM.IRBuilder
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
  trans Program {topBinds, mainExp, topFuncs} = execModuleBuilderT emptyModuleBuilder $ do
    -- topBindsとtopFuncsのOprMapを作成
    bindEnv <-
      fmap fromList
        $ traverse ?? topBinds
        $ \(x, _) ->
          (x,) <$> global (toName x) (convType $ cTypeOf x) (C.Undef (convType $ cTypeOf x))
    let funcEnv =
          fromList
            $ map ?? topFuncs
            $ \(f, (ps, e)) ->
              (f, ConstantOperand $ GlobalReference (ptr $ FunctionType (convType $ cTypeOf e) (map (convType . cTypeOf) ps) False) (toName f))
    runReaderT ?? (bindEnv <> funcEnv :: OprMap) $ evalStateT ?? (mempty :: PrimMap) $ do
      traverse_ (\(f, (ps, body)) -> genFunc f ps body) topFuncs
      void $ function "main" [] LT.i32 $ \_ -> do
        -- topBindsを初期化
        traverse_ loadDef topBinds
        genExp mainExp $ \_ -> ret (int32 0)

type OprMap = IdMap CType Operand

type PrimMap = Map Text Operand

convType :: CType -> Type
convType (ps :-> r) = ptr $ StructureType False [ptr i8, ptr $ FunctionType (convType r) (ptr i8 : map convType ps) False]
convType IntT = i64
convType FloatT = LT.double
convType CharT = i8
convType StringT = ptr i8
convType (PackT cs) = ptr (StructureType False [i64, LT.VectorType (maximum $ sizeofCon <$> toList cs) i8])
convType (ArrayT ty) = ptr $ convType ty
convType AnyT = ptr i64

sizeofCon :: Num a => Con -> a
sizeofCon (Con _ ts) = sum $ map sizeofCType ts

sizeofCType :: Num a => CType -> a
sizeofCType (_ :-> _) = 8
sizeofCType IntT = 8
sizeofCType FloatT = 8
sizeofCType CharT = 1
sizeofCType StringT = 8
sizeofCType (PackT _) = 8
sizeofCType (ArrayT _) = 8
sizeofCType AnyT = 8

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

mallocType ::
  ( MonadState PrimMap m,
    MonadModuleBuilder m,
    MonadIRBuilder m
  ) =>
  Type ->
  m Operand
mallocType ty = mallocBytes (ConstantOperand $ C.sizeof ty) (Just $ ptr ty)

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

genExp ::
  ( MonadReader OprMap m,
    MonadState PrimMap m,
    MonadIRBuilder m,
    MonadModuleBuilder m,
    MonadUniq m
  ) =>
  Exp (Id CType) ->
  (Operand -> m a) ->
  m a
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
genExp (Match e cs) k = genExp e $ \eOpr ->
  case cTypeOf e of
    PackT union -> do
      labels <- traverse (genUnpack _ union k) cs
      _
    _ -> case cs of
      (Bind x body :| _) -> local (at x ?~ eOpr) $ genExp body k
      _ -> bug Unreachable

genUnpack ::
  ( MonadReader OprMap m,
    MonadUniq m,
    MonadModuleBuilder m,
    MonadState PrimMap m,
    MonadIRBuilder m
  ) =>
  Operand ->
  Set Con ->
  (Operand -> m a) ->
  Case (Id CType) ->
  m (Either LLVM.AST.Name (Int, LLVM.AST.Name))
genUnpack scrutinee cs k = \case
  Bind x e -> do
    label <- block
    void $ local (at x ?~ scrutinee) $ genExp e k
    pure $ Left label
  Unpack con vs e -> do
    label <- block
    let (tag, conType) = genCon cs con
    env <- _ conType vs
    void $ local (env <>) $ genExp e k
    pure $ Right (tag, label)

genAtom ::
  ( MonadReader OprMap m,
    MonadUniq m,
    MonadModuleBuilder m
  ) =>
  Atom (Id CType) ->
  m Operand
genAtom (Var x) = findVar x
genAtom (Unboxed (Core.Int x)) = pure $ int64 x
genAtom (Unboxed (Core.Float x)) = pure $ double x
genAtom (Unboxed (Core.Char x)) = pure $ int8 $ toInteger $ ord x
genAtom (Unboxed (Core.String x)) = do
  i <- getUniq
  ConstantOperand <$> globalStringPtr x (LLVM.AST.mkName $ "str" <> show i)

loadDef ::
  ( MonadReader OprMap m,
    MonadModuleBuilder m,
    MonadIRBuilder m,
    MonadState PrimMap m,
    MonadUniq m
  ) =>
  (Id CType, Obj (Id CType)) ->
  m ()
loadDef (name, obj) = do
  addr <- findVar name
  genObj addr (cTypeOf name) obj

genObj ::
  ( MonadModuleBuilder m,
    MonadIRBuilder m,
    MonadReader OprMap m,
    MonadState PrimMap m,
    MonadUniq m
  ) =>
  Operand ->
  CType ->
  Obj (Id CType) ->
  m ()
genObj addr _ (Fun ps e) = do
  name <- freshUnName
  func <- function name (map (,NoParameterName) psTypes) retType $ \case
    [] -> bug Unreachable
    (rawCapture : ps') -> do
      capture <- bitcast rawCapture (ptr capType)
      env <- fmap fromList $ ifor fvs $ \i fv -> do
        capAddr <- gep capture [int32 0, int32 $ fromIntegral i]
        (fv,) <$> load capAddr 0
      let env' = fromList $ zip ps ps'
      local ((env <> env') <>) $ genExp e ret
  capture <- mallocType capType
  ifor_ fvs $ \i fv -> do
    fvOpr <- findVar fv
    capAddr <- gep capture [int32 0, int32 $ fromIntegral i]
    store capAddr 0 fvOpr
  closCapAddr <- gep addr [int32 0, int32 0]
  store closCapAddr 0 =<< bitcast capture (ptr i8)
  closFunAddr <- gep addr [int32 0, int32 1]
  store closFunAddr 0 func
  where
    fvs = toList $ freevars (Fun ps e)
    capType = StructureType False (map (convType . cTypeOf) fvs)
    psTypes = ptr i8 : map (convType . cTypeOf) ps
    retType = convType $ cTypeOf e
genObj addr (PackT cs) (Pack con xs) = do
  let tag = fromIntegral $ Set.findIndex con cs
  tagAddr <- gep addr [int32 0, int32 0]
  store tagAddr 0 (int32 tag)
  ifor_ xs $ \i x -> do
    xAddr <- gep addr [int32 0, int32 1, int32 $ fromIntegral i]
    store xAddr 0 =<< genAtom x
genObj _ _ Pack {} = bug Unreachable
genObj addr _ (Core.Array a n) = do
  sizeOpr <- mul (ConstantOperand $ C.sizeof $ convType $ cTypeOf a) =<< genAtom n
  valueOpr <- mallocBytes sizeOpr (Just $ ptr $ convType $ cTypeOf a)
  store addr 0 =<< load valueOpr 0

genCon :: Set Con -> Con -> (Int, Type)
genCon cs con@(Con _ ts) =
  let idx = Set.findIndex con cs
   in (idx, StructureType False (map convType ts))
