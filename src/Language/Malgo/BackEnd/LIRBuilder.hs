{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.BackEnd.LIRBuilder where

import           Control.Lens                 (snoc)
import           Language.Malgo.ID
import           Language.Malgo.IR.LIR        as L
import           Language.Malgo.IR.MIR        as M
import           Language.Malgo.Monad
import           Language.Malgo.Pretty
import           Language.Malgo.TypeRep.LType as L
import           Language.Malgo.TypeRep.Type  as M
import           Relude                       hiding (Type)
import           Relude.Extra.Map             hiding (size)

newtype ProgramEnv = ProgramEnv { functionMap :: Map (ID Type) (ID LType)
                                }

data ExprEnv = ExprEnv { partialBlockInsts :: IORef [(ID LType, Inst (ID LType))]
                       , variableMap       :: Map (ID Type) (ID LType)
                       , nameHint          :: Text
                       , captures          :: Maybe (ID LType) }

type GenProgram = ReaderT ProgramEnv MalgoM
type GenExpr = ReaderT ExprEnv GenProgram

runGenProgram :: a -> GenProgram [L.Func a] -> MalgoM (L.Program a)
runGenProgram mainFunc m = do
  defs <- runReaderT m (ProgramEnv mempty)
  pure $ L.Program { L.functions = defs, mainFunc = mainFunc }

runGenExpr ::
  Map (ID Type) (ID LType)
  -> Text
  -> GenExpr a
  -> GenProgram (Block (ID LType))
runGenExpr variableMap nameHint m = do
  psRef <- newIORef []
  _ <- runReaderT m (ExprEnv psRef variableMap nameHint Nothing)
  ps <- readIORef psRef
  pure $ Block { insts = reverse ps }

addInst :: Inst (ID LType) -> GenExpr (ID LType)
addInst inst = do
  ExprEnv { partialBlockInsts, nameHint } <- ask
  i <- newID (ltypeOf inst) nameHint
  modifyIORef partialBlockInsts (\s -> (i, inst) : s)
  pure i

findVar :: ID Type -> GenExpr (ID LType)
findVar x = do
  ExprEnv { variableMap } <- ask
  case lookup x variableMap of
    Just x' -> pure x'
    Nothing -> error $ show $ "findVar " <+> pPrint x

findFun :: ID Type -> GenProgram (ID LType)
findFun x = do
  ProgramEnv { functionMap } <- ask
  case lookup x functionMap of
    Just x' -> pure x'
    Nothing -> error $ show $ "findFun " <+> pPrint x

setHint x m = local (\s -> s { nameHint = x }) m

alloca ty msize = addInst $ Alloca ty msize
loadC ptr xs = addInst $ LoadC ptr xs
load ptr xs = addInst $ Load ptr xs
storeC ptr xs val = addInst $ StoreC ptr xs val
store ptr xs val = addInst $ Store ptr xs val
call f xs = addInst $ Call f xs
callExt f retTy xs = addInst $ CallExt f retTy xs
cast ty val = addInst $ Cast ty val
undef ty = addInst $ Undef ty
binop op x y = addInst $ L.BinOp op x y
branchIf c genWhenTrue genWhenFalse = do
  tBlockRef <- newIORef []
  fBlockRef <- newIORef []
  _ <- local (\s -> s { partialBlockInsts = tBlockRef }) genWhenTrue
  _ <- local (\s -> s { partialBlockInsts = fBlockRef }) genWhenFalse
  tBlock <- readIORef tBlockRef
  fBlock <- readIORef fBlockRef

  addInst $ L.If c (Block tBlock) (Block fBlock)

convertType :: HasCallStack => Type -> LType
convertType (TyApp FunC (r:ps)) =
  Ptr $ Struct [Function (convertType r) (Boxed : map convertType ps), Boxed]
convertType (TyApp IntC []) = I64
convertType (TyApp FloatC []) = F64
convertType (TyApp BoolC []) = Bit
convertType (TyApp CharC []) = U8
convertType (TyApp StringC []) = Ptr U8
convertType (TyApp TupleC xs) = Ptr $ Struct $ map convertType xs
convertType (TyApp ArrayC [x]) = Ptr $ convertType x
convertType t = error $ fromString $ "unreachable(convertType): " <> dumpStr t
