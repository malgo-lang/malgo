{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.BackEnd.LIRBuilder where

import           Control.Exception            (assert)
import           Language.Malgo.ID
import           Language.Malgo.IR.LIR        as L
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
  -> GenExpr (ID LType)
  -> GenProgram (Block (ID LType))
runGenExpr variableMap nameHint m = do
  psRef <- newIORef []
  value <- runReaderT m (ExprEnv psRef variableMap nameHint Nothing)
  ps <- readIORef psRef
  pure $ Block { insts = reverse ps, value = value }

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

setHint :: MonadReader ExprEnv m => Text -> m a -> m a
setHint x = local (\s -> s { nameHint = x })

alloca :: LType -> Maybe (ID LType) -> GenExpr (ID LType)
alloca ty msize = addInst $ Alloca ty msize

loadC :: ID LType -> [Int] -> GenExpr (ID LType)
loadC ptr xs = addInst $ LoadC ptr xs

load :: ID LType -> ID LType -> GenExpr (ID LType)
load ptr xs = addInst $ Load ptr xs

storeC :: ID LType -> [Int] -> ID LType -> GenExpr (ID LType)
storeC ptr xs val = addInst $ StoreC ptr xs val

store :: ID LType -> [ID LType] -> ID LType -> GenExpr (ID LType)
store ptr xs val = addInst $ Store ptr xs val

call :: HasCallStack => ID LType -> [ID LType] -> ReaderT ExprEnv GenProgram (ID LType)
call f xs = do
  case (ltypeOf f, map ltypeOf xs) of
    (Function _ ps, as) -> assert (ps == as) (pure ())
    _                   -> error "function must be typed as function"
  addInst $ Call f xs
callExt :: Text -> LType -> [ID LType] -> ReaderT ExprEnv GenProgram (ID LType)
callExt f funTy xs = do
  case (funTy, map ltypeOf xs) of
    (Function _ ps, as) -> assert (ps == as) (pure ())
    _                   -> error "external function must be typed as function"
  addInst $ CallExt f funTy xs

cast :: LType -> ID LType -> GenExpr (ID LType)
cast ty val = addInst $ Cast ty val

undef :: LType -> GenExpr (ID LType)
undef ty = addInst $ Undef ty

binop :: L.Op -> ID LType -> ID LType -> GenExpr (ID LType)
binop op x y = addInst $ L.BinOp op x y

branchIf :: ID LType
              -> GenExpr (ID LType)
              -> GenExpr (ID LType)
              -> GenExpr (ID LType)
branchIf c genWhenTrue genWhenFalse = do
  tBlockRef <- newIORef []
  fBlockRef <- newIORef []
  tvalue <- local (\s -> s { partialBlockInsts = tBlockRef }) genWhenTrue
  fvalue <- local (\s -> s { partialBlockInsts = fBlockRef }) genWhenFalse
  tBlock <- reverse <$> readIORef tBlockRef
  fBlock <- reverse <$> readIORef fBlockRef

  addInst $ L.If c (Block tBlock tvalue) (Block fBlock fvalue)

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
