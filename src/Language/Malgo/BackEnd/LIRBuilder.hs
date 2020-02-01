{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.Malgo.BackEnd.LIRBuilder
  ( MonadProgramBuilder(..)
  , ProgramBuilderT
  , runProgramBuilderT
  , MonadExprBuilder(..)
  , ExprBuilderT
  , ExprEnv(..)
  , runExprBuilderT
  , arrayCreate
  , alloca
  , loadC
  , load
  , storeC
  , store
  , call
  , callExt
  , cast
  , trunc
  , zext
  , sext
  , undef
  , binop
  , branchIf
  , forLoop
  , convertType
  , packClosure
  , coerceTo
  )
where

import           Control.Lens.Indexed           ( iforM_ )
import           Language.Malgo.ID
import           Language.Malgo.Monad
import           Language.Malgo.Prelude
import           Language.Malgo.Pretty
import           Language.Malgo.TypeRep.LType
import           Language.Malgo.TypeRep.Type
import           Language.Malgo.IR.LIR
import           Relude.Unsafe                  ( fromJust )
import           Text.PrettyPrint.HughesPJClass ( Style(mode)
                                                , renderStyle
                                                , style
                                                , Mode(OneLineMode)
                                                )

newtype ProgramEnv = ProgramEnv { functionMap :: IDMap Type (ID LType) }
  deriving newtype (Semigroup, Monoid)

instance One ProgramEnv where
  type OneItem ProgramEnv = (ID Type, ID LType)
  one = ProgramEnv . one

newtype ProgramState = ProgramState { functionList :: DiffList (Func (ID LType)) }

data ExprEnv = ExprEnv { variableMap :: IDMap Type (ID LType)
                       , currentCaptures :: Maybe (ID LType)
                       }

newtype ExprState = ExprState { partialBlockInsts :: DiffList (ID LType, Inst (ID LType)) }

-- Program Builder
class Monad m => MonadProgramBuilder m where
  findFunc :: ID Type -> m (ID LType)
  addFunc :: Func (ID LType) -> m ()

newtype ProgramBuilderT m a = ProgramBuilderT (ReaderT ProgramEnv (StateT ProgramState m) a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadMalgo, MonadUniq)

runProgramBuilderT :: Monad m
                   => ProgramEnv
                   -> ProgramBuilderT m (Block (ID LType))
                   -> m (Program (ID LType))
runProgramBuilderT env (ProgramBuilderT m) = do
  (mf, ProgramState { functionList }) <- runStateT (runReaderT m env) (ProgramState mempty)
  pure $ Program { functions = toList functionList, mainFunc = mf }

instance Monad m => MonadProgramBuilder (ProgramBuilderT m) where
  findFunc x = ProgramBuilderT $ fromJust . lookup x <$> asks functionMap
  addFunc fun = ProgramBuilderT $ modify (\e -> e { functionList = cons fun $ functionList e })

-- Expr Builder
class MonadProgramBuilder m => MonadExprBuilder m where
  findVar :: ID Type -> m (ID LType)
  withVariables :: IDMap Type (ID LType) -> m a -> m a
  addInst :: Inst (ID LType) -> m (ID LType)
  localBlock :: m (ID LType) -> m (Block (ID LType))
  getCurrentCaptures :: m (Maybe (ID LType))

newtype ExprBuilderT m a = ExprBuilderT (ReaderT ExprEnv (StateT ExprState m) a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadMalgo, MonadUniq)

runExprBuilderT :: Monad m => ExprEnv -> ExprBuilderT m (ID LType) -> m (Block (ID LType))
runExprBuilderT env (ExprBuilderT m) = do
  (val, ExprState { partialBlockInsts }) <- runStateT (runReaderT m env) (ExprState mempty)
  pure $ Block { insts = toList partialBlockInsts, value = val }

instance (Monad m, MonadProgramBuilder m) => MonadProgramBuilder (ExprBuilderT m) where
  findFunc x = ExprBuilderT $ lift $ lift $ findFunc x
  addFunc fun = ExprBuilderT $ lift $ lift $ addFunc fun

instance (MonadUniq m, MonadMalgo m, MonadProgramBuilder m) => MonadExprBuilder (ExprBuilderT m) where
  findVar x = ExprBuilderT $ fromJust . lookup x <$> asks variableMap
  withVariables varMap (ExprBuilderT m) =
    ExprBuilderT $ local (\e -> e { variableMap = varMap <> variableMap e }) m
  addInst inst = ExprBuilderT $ do
    i <- newID (ltypeOf inst) "%"
    liftMalgo
      $   logDebug
      $   toText
      $   renderStyle (style { mode = OneLineMode })
      $   pPrint i
      <+> "="
      <+> pPrint inst
    modify (\e -> e { partialBlockInsts = snoc (partialBlockInsts e) (i, inst) })
    pure i
  localBlock (ExprBuilderT m) = ExprBuilderT $ localState $ do
    put (ExprState mempty)
    retval <- m
    insts  <- toList <$> gets partialBlockInsts
    pure (Block insts retval)
  getCurrentCaptures = ExprBuilderT $ asks currentCaptures

-- instructions
arrayCreate :: MonadExprBuilder m => LType -> ID LType -> m (ID LType)
arrayCreate init size = addInst $ ArrayCreate init size

alloca :: MonadExprBuilder m => LType -> m (ID LType)
alloca ty = addInst $ Alloca ty

loadC :: MonadExprBuilder m => ID LType -> [Int] -> m (ID LType)
loadC ptr xs = addInst $ LoadC ptr xs

load :: MonadExprBuilder m => ID LType -> ID LType -> m (ID LType)
load ptr xs = addInst $ Load ptr xs

storeC :: MonadExprBuilder m => ID LType -> [Int] -> ID LType -> m (ID LType)
storeC ptr xs val = addInst (StoreC ptr xs val)

store :: MonadExprBuilder m => ID LType -> [ID LType] -> ID LType -> m (ID LType)
store ptr xs val = addInst (Store ptr xs val)

call :: (MonadUniq m, MonadExprBuilder m) => ID LType -> [ID LType] -> m (ID LType)
call f xs = case ltypeOf f of
  Function _ ps -> do
    as <- zipWithM coerceTo ps xs
    addInst $ Call f as
  _ -> error "function must be typed as function"

callExt :: (MonadUniq m, MonadExprBuilder m) => String -> LType -> [ID LType] -> m (ID LType)
callExt f funTy xs = case funTy of
  Function _ ps -> do
    as <- zipWithM coerceTo ps xs
    addInst $ CallExt f funTy as
  _ -> error "external function must be typed as function"

cast :: MonadExprBuilder m => LType -> ID LType -> m (ID LType)
cast ty val = addInst $ Cast ty val

trunc :: MonadExprBuilder m => LType -> ID LType -> m (ID LType)
trunc ty val = addInst $ Trunc ty val

zext :: MonadExprBuilder m => LType -> ID LType -> m (ID LType)
zext ty val = addInst $ Zext ty val

sext :: MonadExprBuilder m => LType -> ID LType -> m (ID LType)
sext ty val = addInst $ Sext ty val

undef :: MonadExprBuilder m => LType -> m (ID LType)
undef ty = addInst $ Undef ty

binop :: MonadExprBuilder m => Op -> ID LType -> ID LType -> m (ID LType)
binop o x y = addInst $ BinOp o x y

branchIf :: MonadExprBuilder m => ID LType -> m (ID LType) -> m (ID LType) -> m (ID LType)
branchIf c genWhenTrue genWhenFalse = do
  tBlock <- localBlock genWhenTrue
  fBlock <- localBlock genWhenFalse
  addInst $ If c tBlock fBlock

forLoop :: (MonadUniq m, MonadExprBuilder m)
        => ID LType
        -> ID LType
        -> (ID LType -> m (ID LType))
        -> m (ID LType)
forLoop from to k = do
  index <- newID I64 "$i"
  block <- localBlock (k index)
  addInst $ For index from to block

convertType :: HasCallStack => Type -> LType
convertType (TyApp FunC (r : ps)) =
  Ptr (Struct [Function (convertType r) (Ptr U8 : map convertType ps), Ptr U8])
convertType (TyApp IntC    [] ) = I64
convertType (TyApp FloatC  [] ) = F64
convertType (TyApp BoolC   [] ) = Bit
convertType (TyApp CharC   [] ) = U8
convertType (TyApp StringC [] ) = Ptr U8
convertType (TyApp TupleC  xs ) = Ptr $ Struct $ map convertType xs
convertType (TyApp ArrayC  [x]) = Ptr $ Struct [Ptr $ convertType x, SizeT]
convertType TyMeta{}            = Ptr U8
convertType t                   = error $ toText $ "unreachable(convertType): " <> pShow t

packClosure :: MonadExprBuilder m => ID LType -> ID LType -> m (ID LType)
packClosure capsId f = do
  clsId <- alloca (Struct [ltypeOf f, Ptr U8])
  _     <- storeC clsId [0, 0] f
  _     <- storeC clsId [0, 1] capsId
  pure clsId

coerceTo :: (MonadUniq m, MonadExprBuilder m) => LType -> ID LType -> m (ID LType)
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
  (Ptr U8, Bit  ) -> cast (Ptr U8) =<< zext U64 x
  (Ptr U8, I32  ) -> cast (Ptr U8) =<< sext I64 x
  (Ptr U8, I64  ) -> cast (Ptr U8) x
  (Ptr U8, U32  ) -> cast (Ptr U8) =<< zext U64 x
  (Ptr U8, U64  ) -> cast (Ptr U8) x
  (Ptr U8, F64  ) -> cast (Ptr U8) x
  (Ptr U8, SizeT) -> cast (Ptr U8) x
  -- unwrap
  -- closure
  (Ptr (Struct [Function r (Ptr U8 : ps), Ptr U8]), xty) -> do
    -- generate new captured environment
    -- 'f' captures the original closure
    (boxedX, xps, unboxedXType) <- case xty of
      Ptr (Struct [Function _ (Ptr U8 : xs), Ptr U8]) -> (, xs, xty) <$> cast (Ptr U8) x
      Ptr U8 -> pure
        ( x
        , replicate (length ps) (Ptr U8)
        , Ptr (Struct [Function (Ptr U8) (replicate (length ps + 1) (Ptr U8)), Ptr U8])
        )
      _ -> error $ toText $ pShow x <> " is not closure"
    -- generate 'f'
    fName       <- newID (Function r (Ptr U8 : ps)) "$fo"
    fBoxedXName <- newID (Ptr U8) "$x"
    fParamNames <- mapM (`newID` "$a") ps

    bodyBlock   <- localBlock $ do
      fUnboxedX <- cast unboxedXType fBoxedXName
      as        <- zipWithM coerceTo xps fParamNames
      xFun      <- loadC fUnboxedX [0, 0]
      xCap      <- loadC fUnboxedX [0, 1]
      retVal    <- call xFun (xCap : as)
      coerceTo r retVal
    addFunc $ Func { name = fName, params = fBoxedXName : fParamNames, body = bodyBlock }
    packClosure boxedX fName
  -- array
  (Ptr (Struct [Ptr ty, SizeT]), xty) -> do
    x' <- case xty of
      Ptr U8 -> cast (Ptr (Struct [Ptr (Ptr U8), SizeT])) x
      Ptr (Struct [Ptr _, SizeT]) -> pure x
      _ -> error $ toText $ "cannot convert " <> pShow x <> "\n to " <> pShow
        (Ptr (Struct [Ptr ty, SizeT]))
    xRaw      <- loadC x' [0, 0]
    size      <- loadC x' [0, 1]
    newArr    <- arrayCreate ty size
    newArrRaw <- loadC newArr [0, 0]
    void $ storeC newArr [0, 1] size
    zero <- addInst $ Constant $ Int64 0
    void $ forLoop zero size $ \i -> do
      val  <- load xRaw i
      val' <- coerceTo ty val
      store newArrRaw [i] val'
    pure newArr
  -- tuple
  (Ptr (Struct ts), xty) -> do
    x' <- case xty of
      Ptr U8 -> cast (Ptr (Struct $ replicate (length ts) (Ptr U8))) x
      Ptr (Struct _) -> pure x
      _ -> error $ toText $ "cannot convert " <> pShow x <> "\n to " <> pShow (Ptr (Struct ts))
    ptr <- alloca (Struct ts)
    iforM_ ts $ \i ty -> do
      xElem  <- loadC x' [0, 0]
      xElem' <- coerceTo ty xElem
      storeC ptr [0, i] xElem'
    pure ptr
  -- other reference types
  (Ptr ty    , Ptr _ ) -> cast (Ptr ty) x
  (Function{}, Ptr U8) -> cast to x
  -- value types
  (Bit       , Ptr U8) -> cast U64 x >>= trunc Bit
  (I32       , Ptr U8) -> cast I64 x >>= trunc I32
  (I64       , Ptr U8) -> cast I64 x
  (U8        , Ptr U8) -> cast U64 x >>= trunc U8
  (U32       , Ptr U8) -> cast U64 x >>= trunc U32
  (U64       , Ptr U8) -> cast U64 x
  (F64       , Ptr U8) -> cast F64 x
  (SizeT     , Ptr U8) -> cast SizeT x
  -- size_t to integer types
  (SizeT     , I64   ) -> cast SizeT x
  (SizeT     , U64   ) -> cast SizeT x
  (_         , _     ) -> error $ toText $ "cannot convert " <> pShow x <> "\n to " <> pShow to
