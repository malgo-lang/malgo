{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.Malgo.BackEnd.LIRBuilder where

import           Language.Malgo.ID
import           Language.Malgo.Monad
import           Language.Malgo.Prelude
import           Language.Malgo.Pretty
import           Language.Malgo.TypeRep.LType
import           Language.Malgo.TypeRep.Type
import           Language.Malgo.IR.LIR
import           Control.Lens                   ( view
                                                , set
                                                , makeLenses
                                                )
import           Relude.Unsafe                  ( fromJust )

data ProgramEnv = ProgramEnv { _functionMap :: IDMap Type (ID LType)
                             , _functionListRef :: IORef [Func (ID LType)]
                             }
makeLenses ''ProgramEnv

data ExprEnv = ExprEnv { _partialBlockInsts :: IORef [(ID LType, Inst (ID LType))]
                       , _variableMap :: IDMap Type (ID LType)
                       , _currentCaptures :: Maybe (ID LType)
                       }
makeLenses ''ExprEnv

type GenProgram = ReaderT ProgramEnv MalgoM
type GenExpr = ReaderT ExprEnv GenProgram

runGenProgram :: GenProgram (Block (ID LType)) -> MalgoM (Program (ID LType))
runGenProgram m = do
  ref  <- newIORef []
  mf   <- runReaderT m (ProgramEnv mempty ref)
  defs <- readIORef ref
  pure $ Program { functions = defs, mainFunc = mf }

runGenExpr :: IDMap Type (ID LType) -> GenExpr (ID LType) -> GenProgram (Block (ID LType))
runGenExpr varMap m = do
  psRef <- newIORef []
  val   <- runReaderT m (ExprEnv psRef varMap Nothing)
  ps    <- readIORef psRef
  pure $ Block { insts = reverse ps, value = val }

findVar :: ID Type -> GenExpr (ID LType)
findVar x = fromJust . lookup x <$> view variableMap

findFun :: ID Type -> GenProgram (ID LType)
findFun x = fromJust . lookup x <$> view functionMap

addFunc :: Func (ID LType) -> GenProgram ()
addFunc fun = do
  logDebug $ fromString $ render $ "register: " $$ pPrint fun
  flip modifyIORef (fun :) =<< view functionListRef

addInst :: Inst (ID LType) -> GenExpr (ID LType)
addInst inst = do
  i <- newID (ltypeOf inst) "%"
  logDebug $ toText $ renderStyle (style { mode = OneLineMode }) $ pPrint i <+> "=" <+> pPrint inst
  flip modifyIORef ((i, inst) :) =<< view partialBlockInsts
  pure i

arrayCreate :: LType -> ID LType -> GenExpr (ID LType)
arrayCreate init size = addInst $ ArrayCreate init size

alloca :: LType -> GenExpr (ID LType)
alloca ty = addInst $ Alloca ty

loadC :: ID LType -> [Int] -> GenExpr (ID LType)
loadC ptr xs = addInst $ LoadC ptr xs

load :: ID LType -> ID LType -> GenExpr (ID LType)
load ptr xs = addInst $ Load ptr xs

storeC :: ID LType -> [Int] -> ID LType -> GenExpr (ID LType)
storeC ptr xs val = addInst (StoreC ptr xs val)

store :: ID LType -> [ID LType] -> ID LType -> GenExpr (ID LType)
store ptr xs val = addInst (Store ptr xs val)

call :: HasCallStack => ID LType -> [ID LType] -> GenExpr (ID LType)
call f xs = case ltypeOf f of
  Function _ ps -> do
    as <- zipWithM coerceTo ps xs
    addInst $ Call f as
  _ -> error "function must be typed as function"

callExt :: Text -> LType -> [ID LType] -> GenExpr (ID LType)
callExt f funTy xs = case funTy of
  Function _ ps -> do
    as <- zipWithM coerceTo ps xs
    addInst $ CallExt f funTy as
  _ -> error "external function must be typed as function"

cast :: LType -> ID LType -> GenExpr (ID LType)
cast ty val = addInst $ Cast ty val

trunc :: LType -> ID LType -> GenExpr (ID LType)
trunc ty val = addInst $ Trunc ty val

zext :: LType -> ID LType -> GenExpr (ID LType)
zext ty val = addInst $ Zext ty val

sext :: LType -> ID LType -> GenExpr (ID LType)
sext ty val = addInst $ Sext ty val

undef :: LType -> GenExpr (ID LType)
undef ty = addInst $ Undef ty

binop :: Op -> ID LType -> ID LType -> GenExpr (ID LType)
binop o x y = addInst $ BinOp o x y

branchIf :: ID LType -> GenExpr (ID LType) -> GenExpr (ID LType) -> GenExpr (ID LType)
branchIf c genWhenTrue genWhenFalse = do
  tBlockRef <- newIORef []
  fBlockRef <- newIORef []
  tvalue    <- local (set partialBlockInsts tBlockRef) genWhenTrue
  fvalue    <- local (set partialBlockInsts fBlockRef) genWhenFalse
  tBlock    <- reverse <$> readIORef tBlockRef
  fBlock    <- reverse <$> readIORef fBlockRef
  addInst $ If c (Block tBlock tvalue) (Block fBlock fvalue)

forLoop :: ID LType -> ID LType -> (ID LType -> GenExpr (ID LType)) -> GenExpr (ID LType)
forLoop from to k = do
  index    <- newID I64 "i"
  blockRef <- newIORef []
  val      <- local (set partialBlockInsts blockRef) (k index)
  block    <- reverse <$> readIORef blockRef
  addInst $ For index from to (Block block val)

convertType :: HasCallStack => Type -> LType
convertType (TyApp FunC (r : ps)) =
  Ptr $ Struct [Function (convertType r) (Ptr U8 : map convertType ps), Ptr U8]
convertType (TyApp IntC    [] ) = I64
convertType (TyApp FloatC  [] ) = F64
convertType (TyApp BoolC   [] ) = Bit
convertType (TyApp CharC   [] ) = U8
convertType (TyApp StringC [] ) = Ptr U8
convertType (TyApp TupleC  xs ) = Ptr $ Struct $ map convertType xs
convertType (TyApp ArrayC  [x]) = Ptr $ Struct [Ptr $ convertType x, SizeT]
convertType TyMeta{}            = Ptr U8
convertType t                   = error $ toText $ "unreachable(convertType): " <> pShow t

packClosure :: ID LType -> ID LType -> ReaderT ExprEnv GenProgram (ID LType)
packClosure f capsId = do
  clsId <- alloca (Struct [ltypeOf f, Ptr U8])
  _     <- storeC clsId [0, 0] f
  _     <- storeC clsId [0, 1] capsId
  pure clsId

coerceTo :: LType -> ID LType -> GenExpr (ID LType)
coerceTo to x = case (to, ltypeOf x) of
  (ty, xty) | ty == xty -> pure x
  -- boxing closure
  (Ptr U8, Ptr (Struct [Function _ (Ptr U8 : ps), Ptr U8])) ->
    cast (Ptr U8)
      =<< coerceTo
            (Ptr (Struct [Function (Ptr U8) (Ptr U8 : replicate (length ps) (Ptr U8)), Ptr U8]))
            x
  -- boxing array
  (Ptr U8, Ptr (Struct [Ptr _, SizeT])) ->
    cast (Ptr U8) =<< coerceTo (Ptr (Struct [Ptr (Ptr U8), SizeT])) x
  (Ptr U8, Ptr (Struct ts)) ->
    cast (Ptr U8) =<< coerceTo (Ptr (Struct (replicate (length ts) (Ptr U8)))) x
  (Ptr U8, Ptr _) -> cast (Ptr U8) x
  (Ptr U8, Bit  ) -> cast (Ptr U8) =<< zext U64 x
  (Ptr U8, I32  ) -> cast (Ptr U8) =<< sext I64 x
  (Ptr U8, I64  ) -> cast (Ptr U8) x
  (Ptr U8, U32  ) -> cast (Ptr U8) =<< zext U64 x
  (Ptr U8, U64  ) -> cast (Ptr U8) x
  (Ptr U8, F64  ) -> cast (Ptr U8) x
  (Ptr U8, SizeT) -> cast (Ptr U8) x
  (Ptr (Struct [Function r (Ptr U8 : ps), Ptr U8]), xty) -> do
    -- generate new captured environment
    -- f captures the original closure
    (boxedX, xps, unboxedXType) <- case xty of
      ClosurePtr _ xs -> (, xs, xty) <$> cast (Ptr U8) x
      Ptr U8          -> pure
        (x, replicate (length ps) (Ptr U8), ClosurePtr (Ptr U8) (replicate (length ps) (Ptr U8)))
      _ -> error $ toText $ pShow x <> " is not closure"
    -- generate f
    fName       <- newID (Function r (Ptr U8 : ps)) "fo"
    fBoxedXName <- newID (Ptr U8) "x"
    fParamNames <- mapM (`newID` "a") ps
    bodyBlock   <- lift $ runGenExpr mempty $ do
      fUnboxedX <- cast unboxedXType fBoxedXName
      as        <- zipWithM coerceTo xps fParamNames
      xFun      <- loadC fUnboxedX [0, 0]
      xCap      <- loadC fUnboxedX [0, 1]
      retVal    <- call xFun (xCap : as)
      coerceTo r retVal
    lift $ addFunc $ Func { name = fName, params = fBoxedXName : fParamNames, body = bodyBlock }
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
      val <- load xRaw i
      store newArrRaw [i] =<< coerceTo ty val
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
