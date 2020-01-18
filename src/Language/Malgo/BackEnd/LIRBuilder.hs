{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.Malgo.BackEnd.LIRBuilder
  ( GenProgram
  , GenExpr
  , runGenProgram
  , functionMap
  , addFunc
  , runGenExpr
  , variableMap
  , currentCaptures
  , convertType
  , coerceTo
  , packClosure
  , findVar
  , findFun
  , addInst
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
  )
where

import           Language.Malgo.ID
import           Language.Malgo.Monad
import           Language.Malgo.Prelude
import           Language.Malgo.Pretty
import           Language.Malgo.TypeRep.LType
import           Language.Malgo.TypeRep.Type
import           Language.Malgo.IR.LIR
import           Control.Lens                   ( view
                                                , modifying
                                                , makeLenses
                                                )
import           Relude.Unsafe                  ( fromJust )

newtype ProgramEnv = ProgramEnv { _functionMap :: IDMap Type (ID LType) }
makeLenses ''ProgramEnv

newtype ProgramState = ProgramState { _functionList :: Endo [Func (ID LType)] }
makeLenses ''ProgramState

data ExprEnv = ExprEnv { _variableMap :: IDMap Type (ID LType)
                       , _currentCaptures :: Maybe (ID LType)
                       }
makeLenses ''ExprEnv

newtype ExprState = ExprState { _partialBlockInsts :: Endo [(ID LType, Inst (ID LType))] }
makeLenses ''ExprState

type GenProgram = ReaderT ProgramEnv (StateT ProgramState MalgoM)
type GenExpr = ReaderT ExprEnv (StateT ExprState GenProgram)

runGenProgram :: ReaderT ProgramEnv (StateT ProgramState MalgoM) (Block (ID LType))
              -> MalgoM (Program (ID LType))
runGenProgram m = do
  (mf, ProgramState { _functionList }) <- runStateT (runReaderT m (ProgramEnv mempty))
                                                    (ProgramState mempty)
  pure $ Program { functions = appEndo _functionList [], mainFunc = mf }

runGenExpr :: IDMap Type (ID LType)
           -> ReaderT ExprEnv (StateT ExprState GenProgram) (ID LType)
           -> GenProgram (Block (ID LType))
runGenExpr varMap m = do
  (val, ExprState { _partialBlockInsts = ps }) <- runStateT
    (runReaderT m (ExprEnv varMap Nothing))
    (ExprState mempty)
  pure $ Block { insts = appEndo ps [], value = val }

findVar :: MonadReader ExprEnv f => ID Type -> f (ID LType)
findVar x = fromJust . lookup x <$> view variableMap

findFun :: MonadReader ProgramEnv f => ID Type -> f (ID LType)
findFun x = fromJust . lookup x <$> view functionMap

addFunc :: (MonadMalgo m, MonadState ProgramState m) => Func (ID LType) -> m ()
addFunc fun = do
  liftMalgo $ logDebug $ fromString $ render $ "register: " $$ pPrint fun
  modifying functionList (Endo (fun :) <>)

addInst :: (MonadMalgo m, MonadState ExprState m) => Inst (ID LType) -> m (ID LType)
addInst inst = do
  i <- newID (ltypeOf inst) "%"
  liftMalgo
    $   logDebug
    $   toText
    $   renderStyle (style { mode = OneLineMode })
    $   pPrint i
    <+> "="
    <+> pPrint inst
  modifying partialBlockInsts (<> Endo ((i, inst) :))
  pure i

arrayCreate :: (MonadMalgo m, MonadState ExprState m) => LType -> ID LType -> m (ID LType)
arrayCreate init size = addInst $ ArrayCreate init size

alloca :: (MonadMalgo m, MonadState ExprState m) => LType -> m (ID LType)
alloca ty = addInst $ Alloca ty

loadC :: (MonadMalgo m, MonadState ExprState m) => ID LType -> [Int] -> m (ID LType)
loadC ptr xs = addInst $ LoadC ptr xs

load :: (MonadMalgo m, MonadState ExprState m) => ID LType -> ID LType -> m (ID LType)
load ptr xs = addInst $ Load ptr xs

storeC :: (MonadMalgo m, MonadState ExprState m) => ID LType -> [Int] -> ID LType -> m (ID LType)
storeC ptr xs val = addInst (StoreC ptr xs val)

store :: (MonadMalgo m, MonadState ExprState m)
      => ID LType
      -> [ID LType]
      -> ID LType
      -> m (ID LType)
store ptr xs val = addInst (Store ptr xs val)

call :: ID LType -> [ID LType] -> ReaderT ExprEnv (StateT ExprState GenProgram) (ID LType)
call f xs = case ltypeOf f of
  Function _ ps -> do
    as <- zipWithM coerceTo ps xs
    addInst $ Call f as
  _ -> error "function must be typed as function"

callExt :: String -> LType -> [ID LType] -> ReaderT ExprEnv (StateT ExprState GenProgram) (ID LType)
callExt f funTy xs = case funTy of
  Function _ ps -> do
    as <- zipWithM coerceTo ps xs
    addInst $ CallExt f funTy as
  _ -> error "external function must be typed as function"

cast :: (MonadMalgo m, MonadState ExprState m) => LType -> ID LType -> m (ID LType)
cast ty val = addInst $ Cast ty val

trunc :: (MonadMalgo m, MonadState ExprState m) => LType -> ID LType -> m (ID LType)
trunc ty val = addInst $ Trunc ty val

zext :: (MonadMalgo m, MonadState ExprState m) => LType -> ID LType -> m (ID LType)
zext ty val = addInst $ Zext ty val

sext :: (MonadMalgo m, MonadState ExprState m) => LType -> ID LType -> m (ID LType)
sext ty val = addInst $ Sext ty val

undef :: (MonadMalgo m, MonadState ExprState m) => LType -> m (ID LType)
undef ty = addInst $ Undef ty

binop :: (MonadMalgo m, MonadState ExprState m) => Op -> ID LType -> ID LType -> m (ID LType)
binop o x y = addInst $ BinOp o x y

runLocalBlock :: MonadState ExprState m =>
                   m (ID LType) -> m (Block (ID LType))
runLocalBlock m = do
  backup <- get
  put (ExprState mempty)
  retval <- m
  insts <- flip appEndo [] <$> gets (view partialBlockInsts)
  put backup
  pure (Block insts retval)

branchIf :: (MonadState ExprState m, MonadMalgo m)
         => ID LType
         -> m (ID LType)
         -> m (ID LType)
         -> m (ID LType)
branchIf c genWhenTrue genWhenFalse = do
  tBlock <- runLocalBlock genWhenTrue
  fBlock <- runLocalBlock genWhenFalse
  addInst $ If c tBlock fBlock

forLoop :: (MonadState ExprState m, MonadMalgo m)
        => ID LType
        -> ID LType
        -> (ID LType -> m (ID LType))
        -> m (ID LType)
forLoop from to k = do
  index <- newID I64 "$i"
  block <- runLocalBlock (k index)
  addInst $ For index from to block

convertType :: HasCallStack => Type -> LType
convertType (TyApp FunC    (r : ps)) = ClosurePtr (convertType r) (map convertType ps)
convertType (TyApp IntC    []      ) = I64
convertType (TyApp FloatC  []      ) = F64
convertType (TyApp BoolC   []      ) = Bit
convertType (TyApp CharC   []      ) = U8
convertType (TyApp StringC []      ) = Ptr U8
convertType (TyApp TupleC  xs      ) = Ptr $ Struct $ map convertType xs
convertType (TyApp ArrayC  [x]     ) = Ptr $ Struct [Ptr $ convertType x, SizeT]
convertType TyMeta{}                 = Ptr U8
convertType t                        = error $ toText $ "unreachable(convertType): " <> pShow t

packClosure :: (MonadMalgo m, MonadState ExprState m) => ID LType -> ID LType -> m (ID LType)
packClosure f capsId = do
  clsId <- alloca (Struct [ltypeOf f, Ptr U8])
  _     <- storeC clsId [0, 0] f
  _     <- storeC clsId [0, 1] capsId
  pure clsId

coerceTo :: LType -> ID LType -> ReaderT ExprEnv (StateT ExprState GenProgram) (ID LType)
coerceTo to x = case (to, ltypeOf x) of
  (ty, xty) | ty == xty -> pure x
  -- boxing closure
  (Ptr U8, ClosurePtr _ ps) ->
    cast (Ptr U8) =<< coerceTo (ClosurePtr (Ptr U8) (replicate (length ps) (Ptr U8))) x
  -- boxing array
  (Ptr U8, Ptr (Struct [Ptr _, SizeT])) ->
    cast (Ptr U8) =<< coerceTo (Ptr (Struct [Ptr (Ptr U8), SizeT])) x
  (Ptr U8, Ptr (Struct ts)) ->
    cast (Ptr U8) =<< coerceTo (Ptr (Struct (replicate (length ts) (Ptr U8)))) x
  (Ptr U8         , Ptr _) -> cast (Ptr U8) x
  (Ptr U8         , Bit  ) -> cast (Ptr U8) =<< zext U64 x
  (Ptr U8         , I32  ) -> cast (Ptr U8) =<< sext I64 x
  (Ptr U8         , I64  ) -> cast (Ptr U8) x
  (Ptr U8         , U32  ) -> cast (Ptr U8) =<< zext U64 x
  (Ptr U8         , U64  ) -> cast (Ptr U8) x
  (Ptr U8         , F64  ) -> cast (Ptr U8) x
  (Ptr U8         , SizeT) -> cast (Ptr U8) x
  (ClosurePtr r ps, xty  ) -> do
    -- generate new captured environment
    -- f captures the original closure
    (boxedX, xps, unboxedXType) <- case xty of
      ClosurePtr _ xs -> (, xs, xty) <$> cast (Ptr U8) x
      Ptr U8          -> pure
        (x, replicate (length ps) (Ptr U8), ClosurePtr (Ptr U8) (replicate (length ps) (Ptr U8)))
      _ -> error $ toText $ pShow x <> " is not closure"
    -- generate f
    fName       <- newID (Function r (Ptr U8 : ps)) "$fo"
    fBoxedXName <- newID (Ptr U8) "$x"
    fParamNames <- mapM (`newID` "$a") ps

    bodyBlock <- runLocalBlock $ do
      fUnboxedX <- cast unboxedXType fBoxedXName
      as        <- zipWithM coerceTo xps fParamNames
      xFun      <- loadC fUnboxedX [0, 0]
      xCap      <- loadC fUnboxedX [0, 1]
      retVal    <- call xFun (xCap : as)
      coerceTo r retVal
    lift $ lift $ addFunc $ Func { name   = fName
                                 , params = fBoxedXName : fParamNames
                                 , body   = bodyBlock
                                 }
    packClosure fName boxedX
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
  (Ptr (Struct ts), xty) -> do
    x' <- case xty of
      Ptr U8 -> cast (Ptr (Struct $ replicate (length ts) (Ptr U8))) x
      Ptr (Struct _) -> pure x
      _ -> error $ toText $ "cannot convert " <> pShow x <> "\n to " <> pShow (Ptr (Struct ts))
    ptr <- alloca (Struct ts)
    forM_ (zip [0 ..] ts) $ \(i, ty) -> do
      xElem  <- loadC x' [0, 0]
      xElem' <- coerceTo ty xElem
      storeC ptr [0, i] xElem'
    pure ptr
  (SizeT     , I64   ) -> cast SizeT x
  (SizeT     , U64   ) -> cast SizeT x
  (SizeT     , Ptr U8) -> cast SizeT x
  (Ptr ty    , Ptr U8) -> cast (Ptr ty) x
  (Bit       , Ptr U8) -> cast U64 x >>= trunc Bit
  (I32       , Ptr U8) -> cast I64 x >>= trunc I32
  (I64       , Ptr U8) -> cast I64 x
  (U8        , Ptr U8) -> cast U64 x >>= trunc U8
  (U32       , Ptr U8) -> cast U64 x >>= trunc U32
  (U64       , Ptr U8) -> cast U64 x
  (Function{}, Ptr U8) -> cast to x
  (_         , _     ) -> error $ toText $ "cannot convert " <> pShow x <> "\n to " <> pShow to
