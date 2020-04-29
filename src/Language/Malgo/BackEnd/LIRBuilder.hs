{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.BackEnd.LIRBuilder
  ( MonadProgramBuilder (..),
    ProgramBuilderT,
    runProgramBuilderT,
    ProgramEnv (..),
    MonadExprBuilder (..),
    ExprBuilderT,
    ExprEnv (..),
    runExprBuilderT,
    arrayCreate,
    alloca,
    loadC,
    load,
    call,
    callExt,
    cast,
    trunc,
    zext,
    sext,
    undef,
    binop,
    branchIf,
    convertType,
    packClosure,
    coerceTo,
  )
where

import Data.DList (DList (..))
import qualified Data.DList as D
import qualified Data.Text.Lazy as TL
import Language.Malgo.IR.LIR
import Language.Malgo.Id
import Language.Malgo.Monad
import Language.Malgo.Prelude hiding (from, index, to)
import Language.Malgo.Pretty
import Language.Malgo.TypeRep.LType
import Language.Malgo.TypeRep.Type

newtype ProgramEnv = ProgramEnv {functionMap :: IdMap Type (Id LType)}
  deriving newtype (Semigroup, Monoid)

newtype ProgramState = ProgramState {functionList :: DList (Func (Id LType))}

newtype ExprEnv = ExprEnv {currentCaptures :: Maybe (Id LType)}

data ExprState
  = ExprState
      { variableMap :: IdMap Type (Id LType),
        partialBlockInsns :: DList (Insn (Id LType))
      }

-- Program Builder
class Monad m => MonadProgramBuilder m where
  findFunc :: Id Type -> m (Id LType)
  addFunc :: Func (Id LType) -> m ()

newtype ProgramBuilderT m a = ProgramBuilderT (ReaderT ProgramEnv (StateT ProgramState m) a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadMalgo, MonadUniq)

runProgramBuilderT ::
  Monad m =>
  ProgramEnv ->
  ProgramBuilderT m (Block (Id LType)) ->
  m (Program (Id LType))
runProgramBuilderT env (ProgramBuilderT m) = do
  (mf, ProgramState {functionList}) <- runStateT (runReaderT m env) (ProgramState mempty)
  pure $ Program {functions = toList functionList, mainFunc = mf}

instance Monad m => MonadProgramBuilder (ProgramBuilderT m) where
  findFunc x = ProgramBuilderT $ fromJust . view (at x) <$> asks functionMap
  addFunc fun = ProgramBuilderT $ modify (\e -> e {functionList = D.cons fun $ functionList e})

-- Expr Builder
class MonadProgramBuilder m => MonadExprBuilder m where
  findVar :: Id Type -> m (Id LType)
  defineVar :: Id Type -> Id LType -> m ()
  def :: Expr (Id LType) -> m (Id LType)
  storeC :: Id LType -> [Int] -> Id LType -> m ()
  store :: Id LType -> [Id LType] -> Id LType -> m ()
  forLoop :: Id LType -> Id LType -> (Id LType -> m ()) -> m ()
  localBlock :: m (Id LType) -> m (Block (Id LType))
  getCurrentCaptures :: m (Maybe (Id LType))
  replaceVar :: Id LType -> Id LType -> m ()

newtype ExprBuilderT m a = ExprBuilderT {unExprBuilderT :: ReaderT ExprEnv (StateT ExprState m) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadMalgo, MonadUniq)

runExprBuilderT :: MonadUniq m => ExprEnv -> ExprBuilderT m (Id LType) -> m (Block (Id LType))
runExprBuilderT env (ExprBuilderT m) =
  evalStateT ?? (ExprState mempty mempty) $ runReaderT ?? env $ do
    value <- m
    ExprState {partialBlockInsns} <- get
    pure $ Block {insns = toList partialBlockInsns, value = value}

instance (Monad m, MonadProgramBuilder m) => MonadProgramBuilder (ExprBuilderT m) where
  findFunc x = ExprBuilderT $ lift $ lift $ findFunc x
  addFunc fun = ExprBuilderT $ lift $ lift $ addFunc fun

instance (MonadUniq m, MonadProgramBuilder m) => MonadExprBuilder (ExprBuilderT m) where
  findVar x = ExprBuilderT $ fromJust . view (at x) <$> gets variableMap
  defineVar x y =
    ExprBuilderT $ modify (\s -> s {variableMap = set (at x) (Just y) (variableMap s)})

  def expr = ExprBuilderT $ do
    i <- newId (ltypeOf expr) "%"
    modify (\e -> e {partialBlockInsns = D.snoc (partialBlockInsns e) (Assign i expr)})
    pure i

  storeC var is val =
    ExprBuilderT $
      modify (\e -> e {partialBlockInsns = D.snoc (partialBlockInsns e) (StoreC var is val)})

  store var is val =
    ExprBuilderT $
      modify (\e -> e {partialBlockInsns = D.snoc (partialBlockInsns e) (Store var is val)})

  forLoop from to k = ExprBuilderT $ do
    index <- newId I64 "$i"
    block <- unExprBuilderT $ localBlock (k index >> undef Void) -- TODO: remove undef
    modify (\e -> e {partialBlockInsns = D.snoc (partialBlockInsns e) (For index from to block)})

  localBlock (ExprBuilderT m) = ExprBuilderT $ do
    backup <- get
    block <- do
      put $ backup {partialBlockInsns = mempty}
      retval <- m
      insts <- toList <$> gets partialBlockInsns
      pure (Block insts retval)
    vm <- gets variableMap
    put $ backup {variableMap = vm}
    pure block

  getCurrentCaptures = ExprBuilderT $ asks currentCaptures
  replaceVar x y = ExprBuilderT $ modify $ \s ->
    s {variableMap = (\a -> if x == a then y else a) <$> variableMap s}

-- instructions
arrayCreate :: MonadExprBuilder m => LType -> Id LType -> m (Id LType)
arrayCreate t n = def $ ArrayCreate t n

alloca :: MonadExprBuilder m => LType -> m (Id LType)
alloca ty = def $ Alloca ty

loadC :: MonadExprBuilder m => Id LType -> [Int] -> m (Id LType)
loadC ptr xs = def $ LoadC ptr xs

load :: MonadExprBuilder m => LType -> Id LType -> [Id LType] -> m (Id LType)
load ltype ptr xs = def $ Load ltype ptr xs

call :: (MonadUniq m, MonadExprBuilder m) => Id LType -> [Id LType] -> m (Id LType)
call f xs = case ltypeOf f of
  Function _ ps -> do
    as <- zipWithM coerceTo ps xs
    result <- def $ Call f as
    -- "write back". See also 'coerceTo'.
    zipWithM_ (\x a -> replaceVar x =<< coerceTo (ltypeOf x) a) xs as
    pure result
  _ -> error "function must be typed as function"

callExt :: (MonadUniq m, MonadExprBuilder m) => String -> LType -> [Id LType] -> m (Id LType)
callExt f funTy xs = case funTy of
  Function _ ps -> do
    as <- zipWithM coerceTo ps xs
    result <- def $ CallExt f funTy as
    -- "write back". See also 'coerceTo'.
    zipWithM_ (\x a -> replaceVar x =<< coerceTo (ltypeOf x) a) xs as
    pure result
  _ -> error "external function must be typed as function"

cast :: MonadExprBuilder m => LType -> Id LType -> m (Id LType)
cast ty val = def $ Cast ty val

trunc :: MonadExprBuilder m => LType -> Id LType -> m (Id LType)
trunc ty val = def $ Trunc ty val

zext :: MonadExprBuilder m => LType -> Id LType -> m (Id LType)
zext ty val = def $ Zext ty val

sext :: MonadExprBuilder m => LType -> Id LType -> m (Id LType)
sext ty val = def $ Sext ty val

undef :: MonadExprBuilder m => LType -> m (Id LType)
undef ty = def $ Undef ty

binop :: MonadExprBuilder m => Op -> Id LType -> Id LType -> m (Id LType)
binop o x y = def $ BinOp o x y

branchIf ::
  (MonadUniq m, MonadExprBuilder m) =>
  Id LType ->
  m (Id LType) ->
  m (Id LType) ->
  m (Id LType)
branchIf c genWhenTrue genWhenFalse = do
  tBlock <- localBlock genWhenTrue
  fBlock <- localBlock (genWhenFalse >>= coerceTo (ltypeOf tBlock))
  def $ If c tBlock fBlock

convertType :: Type -> LType
convertType (ps :-> r) =
  Ptr (Struct [Function (convertType r) (Ptr U8 : map convertType ps), Ptr U8])
convertType (TyApp IntC []) = I64
convertType (TyApp FloatC []) = F64
convertType (TyApp BoolC []) = Bit
convertType (TyApp CharC []) = U8
convertType (TyApp StringC []) = Ptr U8
convertType (TyApp TupleC xs) = Ptr $ Struct $ map convertType xs
convertType (TyApp ArrayC [x]) = Ptr $ Struct [Ptr $ convertType x, SizeT]
convertType TyMeta {} = Ptr U8
convertType _ = bug Unreachable

packClosure :: MonadExprBuilder m => Id LType -> Id LType -> m (Id LType)
packClosure capsId f = do
  clsId <- alloca (Struct [ltypeOf f, Ptr U8])
  storeC clsId [0, 0] f
  storeC clsId [0, 1] capsId
  pure clsId

-- 'coerceTo' may create a different entity. To keep the values ​​identical before and after 'coerceTo',
-- you need to apply 'coerceTo' again and “write back”.
coerceTo :: (MonadUniq m, MonadExprBuilder m) => LType -> Id LType -> m (Id LType)
coerceTo to x = case (to, ltypeOf x) of
  (ty, xty) | ty == xty -> pure x
  -- wrap
  -- closure
  (Ptr U8, Ptr (Struct [Function _ (Ptr U8 : ps), Ptr U8])) ->
    cast (Ptr U8)
      =<< coerceTo (Ptr (Struct [Function (Ptr U8) (replicate (length ps + 1) (Ptr U8)), Ptr U8])) x
  -- array
  (Ptr U8, Ptr (Struct [Ptr _, SizeT])) ->
    cast (Ptr U8) =<< coerceTo (Ptr (Struct [Ptr (Ptr U8), SizeT])) x
  -- tuple
  (Ptr U8, Ptr (Struct ts)) ->
    cast (Ptr U8) =<< coerceTo (Ptr (Struct (replicate (length ts) (Ptr U8)))) x
  -- other reference types
  (Ptr U8, Ptr _) -> cast (Ptr U8) x
  -- value types
  (Ptr U8, Bit) -> cast (Ptr U8) =<< zext U64 x
  (Ptr U8, I32) -> cast (Ptr U8) =<< sext I64 x
  (Ptr U8, I64) -> cast (Ptr U8) x
  (Ptr U8, U32) -> cast (Ptr U8) =<< zext U64 x
  (Ptr U8, U64) -> cast (Ptr U8) x
  (Ptr U8, F64) -> cast (Ptr U8) x
  (Ptr U8, SizeT) -> cast (Ptr U8) x
  -- unwrap
  -- closure
  (Ptr (Struct [Function r (Ptr U8 : ps), Ptr U8]), xty) -> do
    -- generate new captured environment
    -- 'f' captures the original closure
    boxedX <- cast (Ptr U8) x
    let (xps, unboxedXType) = case xty of
          Ptr (Struct [Function _ (Ptr U8 : xs), Ptr U8]) -> (xs, xty)
          Ptr U8 ->
            ( replicate (length ps) (Ptr U8),
              Ptr (Struct [Function (Ptr U8) (replicate (length ps + 1) (Ptr U8)), Ptr U8])
            )
          _ -> error $ TL.unpack $ pShow x <> " is not closure"
    -- generate 'f'
    fName <- newId (Function r (Ptr U8 : ps)) "$f"
    fBoxedXName <- newId (Ptr U8) "$x"
    fParamNames <- mapM (`newId` "$a") ps
    bodyBlock <- localBlock $ do
      fUnboxedX <- cast unboxedXType fBoxedXName
      as <- zipWithM coerceTo xps fParamNames
      xFun <- loadC fUnboxedX [0, 0]
      xCap <- loadC fUnboxedX [0, 1]
      retVal <- call xFun (xCap : as)
      coerceTo r retVal
    addFunc $ Func {name = fName, params = fBoxedXName : fParamNames, body = bodyBlock}
    packClosure boxedX fName
  -- array
  (Ptr (Struct [Ptr ty, SizeT]), xty) -> do
    (elemTy, x') <- case xty of
      Ptr U8 -> (Ptr U8,) <$> cast (Ptr (Struct [Ptr (Ptr U8), SizeT])) x
      Ptr (Struct [Ptr t, SizeT]) -> pure (t, x)
      _ ->
        error $ TL.unpack $
          "cannot convert " <> pShow x <> "\n to "
            <> pShow
              (Ptr (Struct [Ptr ty, SizeT]))
    xRaw <- loadC x' [0, 0]
    len <- loadC x' [0, 1]
    newArr <- arrayCreate ty len
    newArrRaw <- loadC newArr [0, 0]
    storeC newArr [0, 1] len
    zero <- def $ Constant $ Int64 0
    forLoop zero len $ \i -> do
      val <- load elemTy xRaw [i]
      val' <- coerceTo ty val
      store newArrRaw [i] val'
    pure newArr
  -- tuple
  (Ptr (Struct ts), xty) -> do
    x' <- case xty of
      Ptr U8 -> cast (Ptr (Struct $ replicate (length ts) (Ptr U8))) x
      Ptr (Struct _) -> pure x
      _ -> error $ TL.unpack $ "cannot convert " <> pShow x <> "\n to " <> pShow (Ptr (Struct ts))
    ptr <- alloca (Struct ts)
    ifor_ ts $ \i ty -> do
      xElem <- loadC x' [0, i]
      xElem' <- coerceTo ty xElem
      storeC ptr [0, i] xElem'
    pure ptr
  -- other reference types
  (Ptr ty, Ptr _) -> cast (Ptr ty) x
  (Function {}, Ptr U8) -> cast to x
  -- value types
  (Bit, Ptr U8) -> cast U64 x >>= trunc Bit
  (I32, Ptr U8) -> cast I64 x >>= trunc I32
  (I64, Ptr U8) -> cast I64 x
  (U8, Ptr U8) -> cast U64 x >>= trunc U8
  (U32, Ptr U8) -> cast U64 x >>= trunc U32
  (U64, Ptr U8) -> cast U64 x
  (F64, Ptr U8) -> cast F64 x
  (SizeT, Ptr U8) -> cast SizeT x
  -- size_t to integer types
  (SizeT, I64) -> cast SizeT x
  (SizeT, U64) -> cast SizeT x
  (_, _) -> error $ TL.unpack $ "cannot convert " <> pShow x <> "\n to " <> pShow to
