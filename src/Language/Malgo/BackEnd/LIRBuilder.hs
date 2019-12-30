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
import           Control.Lens
import           Relude.Unsafe                  ( fromJust )

data ProgramEnv = ProgramEnv { _functionMap :: IDMap Type (ID LType)
                             , _innerFunctions :: IORef [Func (ID LType)]
                             }
makeLenses ''ProgramEnv

data ExprEnv = ExprEnv { _partialBlockInsts :: IORef [(ID LType, Inst (ID LType))]
                       , _variableMap :: IDMap Type (ID LType)
                       , _currentCaptures :: Maybe (ID LType)
                       }
makeLenses ''ExprEnv

type GenProgram = ReaderT ProgramEnv MalgoM
type GenExpr = ReaderT ExprEnv GenProgram

runGenProgram :: ID LType -> GenProgram [Func (ID LType)] -> MalgoM (Program (ID LType))
runGenProgram mainName m = do
  ref       <- newIORef []
  defs      <- runReaderT m (ProgramEnv mempty ref)
  innerDefs <- readIORef ref
  pure $ Program { functions = innerDefs <> defs, mainFunc = mainName }

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

addInnerFunc :: Func (ID LType) -> GenProgram ()
addInnerFunc fun = flip modifyIORef (fun :) =<< view innerFunctions

addInst :: Inst (ID LType) -> GenExpr (ID LType)
addInst inst = do
  i <- newID (ltypeOf inst) "%"
  logDebug $ toText $ renderStyle (style { mode = OneLineMode }) $ pPrint i <+> "=" <+> pPrint inst
  flip modifyIORef ((i, inst) :) =<< view partialBlockInsts
  pure i

arrayCreate :: ID LType -> ID LType -> GenExpr (ID LType)
arrayCreate init size = addInst $ ArrayCreate init size

alloca :: LType -> GenExpr (ID LType)
alloca ty = addInst $ Alloca ty

loadC :: ID LType -> [Int] -> GenExpr (ID LType)
loadC ptr xs = addInst $ LoadC ptr xs

load :: ID LType -> ID LType -> GenExpr (ID LType)
load ptr xs = addInst $ Load ptr xs

storeC :: ID LType -> [Int] -> ID LType -> GenExpr ()
storeC ptr xs val = addInst (StoreC ptr xs val) >> pass

store :: ID LType -> [ID LType] -> ID LType -> GenExpr ()
store ptr xs val = addInst (Store ptr xs val) >> pass

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

convertType :: HasCallStack => Type -> LType
convertType (TyApp FunC (r : ps)) =
  Ptr $ Struct [Function (convertType r) (Ptr U8 : map convertType ps), Ptr U8]
convertType (TyApp IntC    [] ) = I64
convertType (TyApp FloatC  [] ) = F64
convertType (TyApp BoolC   [] ) = Bit
convertType (TyApp CharC   [] ) = U8
convertType (TyApp StringC [] ) = Ptr U8
convertType (TyApp TupleC  xs ) = Ptr $ Struct $ map convertType xs
convertType (TyApp ArrayC  [x]) = Ptr $ convertType x
convertType TyMeta{}            = Ptr U8
convertType t                   = error $ toText $ "unreachable(convertType): " <> pShow t

packClosure :: ID LType -> ID LType -> ReaderT ExprEnv GenProgram (ID LType)
packClosure f capsId = do
  clsId <- alloca (Struct [ltypeOf f, Ptr U8])
  storeC clsId [0, 0] f
  storeC clsId [0, 1] capsId
  pure clsId

-- | convert to boxed value
wrap :: ID LType -> GenExpr (ID LType)
wrap x = case ltypeOf x of
  Ptr U8 -> pure x
  Ptr (Struct [Function _ (Ptr U8 : ps), Ptr U8]) -> do
    -- generate new captured environment
    -- fw captures the original closure
    boxedX       <- cast (Ptr U8) x
    -- generate fw
    fwName       <- newID (Function (Ptr U8) (Ptr U8 : replicate (length ps) (Ptr U8))) "fw"
    fwBoxedXName <- newID (Ptr U8) "boxedCaps"
    fwParamNames <- replicateM (length ps) $ newID (Ptr U8) "a"
    bodyBlock    <- lift $ runGenExpr mempty $ do
      fwUnboxedX <- cast (ltypeOf x) fwBoxedXName
      as         <- zipWithM unwrap ps fwParamNames
      xFun       <- loadC fwUnboxedX [0, 0]
      xCap       <- loadC fwUnboxedX [0, 1]
      retVal     <- call xFun (xCap : as)
      wrap retVal
    lift $ addInnerFunc $ Func { name   = fwName
                               , params = fwBoxedXName : fwParamNames
                               , body   = bodyBlock
                               }
    cast (Ptr U8) =<< packClosure fwName boxedX
  Ptr (Struct ts) -> do
    ptr <- alloca (Struct (replicate (length ts) (Ptr U8)))
    forM_ [0 .. length ts - 1] $ \i -> do
      raw    <- loadC x [0, i]
      wraped <- wrap raw
      storeC ptr [0, i] wraped
    cast (Ptr U8) ptr
  Ptr _ -> cast (Ptr U8) x
  Bit   -> zext U64 x >>= cast (Ptr U8)
  I32   -> sext I64 x >>= cast (Ptr U8)
  I64   -> cast (Ptr U8) x
  U32   -> zext U64 x >>= cast (Ptr U8)
  U64   -> cast (Ptr U8) x
  F64   -> cast (Ptr U8) x
  t     -> errorDoc $ "cannot convert" <+> pPrint t <+> "to boxed value"

-- | convert boxed value to unboxed value
unwrap :: LType -> ID LType -> GenExpr (ID LType)
unwrap (Ptr U8) = pure
unwrap (Ptr (Struct [Function r (Ptr U8 : ps), Ptr U8])) = \x -> do
  -- new capturesd environment is `x`
  -- fo captures the boxed closure
  -- note: ltypeOf x == Ptr U8
  -- generate fo
  foName       <- newID (Function r (Ptr U8 : ps)) "fo"
  foXName      <- newID (Ptr U8) "x"
  foParamNames <- mapM (`newID` "a") ps
  bodyBlock    <- lift $ runGenExpr mempty $ do
    foUnboxedX <- cast
      (Ptr (Struct [Function (Ptr U8) (Ptr U8 : replicate (length ps) (Ptr U8)), Ptr U8]))
      foXName
    wrappedParams <- mapM wrap foParamNames
    xFun          <- loadC foUnboxedX [0, 0]
    xCap          <- loadC foUnboxedX [0, 1]
    retVal        <- call xFun (xCap : wrappedParams)
    unwrap r retVal
  lift $ addInnerFunc $ Func { name = foName, params = foXName : foParamNames, body = bodyBlock }
  packClosure foName x
unwrap (Ptr (Struct ts)) = \x -> do
  x'  <- cast (Ptr $ Struct $ replicate (length ts) (Ptr U8)) x
  ptr <- alloca (Struct ts)
  forM_ (zip [0 ..] ts) $ \(i, t) -> do
    wraped <- loadC x' [0, i]
    raw    <- unwrap t wraped
    storeC ptr [0, i] raw
  pure ptr
unwrap (Ptr t)      = cast (Ptr t)
unwrap Bit          = cast U64 >=> trunc Bit
unwrap I32          = cast I64 >=> trunc I32
unwrap I64          = cast I64
unwrap U8           = cast U64 >=> trunc U8
unwrap U32          = cast U64 >=> trunc U32
unwrap U64          = cast U64
unwrap F64          = cast F64
unwrap t@Function{} = cast t
unwrap Struct{}     = error "cannot convert boxed value to Struct"
unwrap Void         = error "cannot convert boxed value to Void"

coerceTo :: HasCallStack => LType -> ID LType -> GenExpr (ID LType)
coerceTo t x | t == ltypeOf x = pure x
coerceTo (Ptr U8) x           = wrap x
coerceTo (Ptr (Struct [Function r (Ptr U8 : ps), Ptr U8])) x = case ltypeOf x of
  Ptr (Struct [Function _ (Ptr U8 : xps), Ptr U8]) -> do
    -- generate new captured environment
    -- f captures the original closure
    boxedX      <- cast (Ptr U8) x

    -- generate f
    fName       <- newID (Function r (Ptr U8 : ps)) "f"
    fBoxedXName <- newID (Ptr U8) "boxedCaps"
    fParamNames <- mapM (\p -> newID (ltypeOf p) "p") ps
    bodyBlock   <- lift $ runGenExpr mempty $ do
      fUnboxedX <- cast (ltypeOf x) fBoxedXName
      as        <- zipWithM coerceTo xps fParamNames
      xFun      <- loadC fUnboxedX [0, 0]
      xCap      <- loadC fUnboxedX [0, 1]
      retVal    <- call xFun (xCap : as)
      coerceTo r retVal
    lift $ addInnerFunc $ Func { name   = fName
                               , params = fBoxedXName : fParamNames
                               , body   = bodyBlock
                               }
    packClosure fName boxedX
  Ptr U8 -> unwrap (Ptr (Struct [Function r (Ptr U8 : ps), Ptr U8])) x
  _      -> error $ toText $ pShow x <> " is not closure"
coerceTo (Ptr (Struct ts)) x = case ltypeOf x of
  Ptr U8         -> unwrap (Ptr (Struct ts)) x
  Ptr (Struct _) -> do
    ptr <- alloca (Struct ts)
    forM_ (zip [0 ..] ts) $ \(i, t) -> do
      xElem  <- loadC x [0, 0]
      xElem' <- coerceTo t xElem
      storeC ptr [0, i] xElem'
    pure ptr
  _ -> error $ toText $ "cannot convert " <> pShow x <> "\n to " <> pShow (Ptr (Struct ts))
coerceTo t x = case ltypeOf x of
  Ptr U8 -> unwrap t x
  _      -> error $ toText $ "cannot convert " <> pShow x <> "\n to " <> pShow t
