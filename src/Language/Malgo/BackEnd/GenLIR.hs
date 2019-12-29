{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.BackEnd.GenLIR where

import qualified Data.ByteString               as B
import           Language.Malgo.ID
import           Language.Malgo.IR.HIR         as H
                                                ( Lit(..)
                                                , Op(..)
                                                )
import           Language.Malgo.IR.LIR         as L
import           Language.Malgo.IR.MIR         as M
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Pretty
import           Language.Malgo.TypeRep.LType  as L
import           Language.Malgo.TypeRep.Type   as M
import           Language.Malgo.Prelude
import           Relude.Unsafe                  ( fromJust )

data GenLIR

instance Pass GenLIR (M.Program Type (ID Type)) (L.Program (ID LType)) where
  isDump = dumpLIR
  trans M.Program { functions, mainExpr } = do
    mainFuncId    <- newID (Function I32 []) "main"
    funMap        <- foldMapM genFunMap functions
    innerFuncsRef <- newIORef []
    prog          <-
      runGenProgram mainFuncId innerFuncsRef
      $ local (\s -> s { functionMap = funMap })
      $ do
          fs <- mapM genFunction functions
          mf <- genMainFunction mainFuncId mainExpr
          pure (mf : fs)
    innerFuncs <- readIORef innerFuncsRef
    pure
      ((prog :: L.Program (ID LType)) { functions = innerFuncs
                                        <> L.functions prog
                                      }
      )
   where
    genFunMap M.Func { name, captures } = case typeOf name of
      TyApp FunC (r : ps) -> do
        newName <- newID (functionType (isNothing captures) ps r) (idName name)
        pure $ one (name, newName)
      _ -> error "genFunMap"
    functionType isKnown ps r
      | isKnown = Function (convertType $ typeOf r)
                           (map (convertType . typeOf) ps)
      | otherwise = Function (convertType $ typeOf r)
                             (Ptr U8 : map (convertType . typeOf) ps)

data ProgramEnv = ProgramEnv { functionMap :: IDMap Type (ID LType)
                             , innerFunctions :: IORef [L.Func (ID LType)]
                             }

data ExprEnv = ExprEnv { partialBlockInsts :: IORef [(ID LType, Inst (ID LType))]
                       , variableMap       :: IDMap Type (ID LType)
                       , captures          :: Maybe (ID LType) }

type GenProgram = ReaderT ProgramEnv MalgoM
type GenExpr = ReaderT ExprEnv GenProgram

runGenProgram
  :: a
  -> IORef [L.Func (ID LType)]
  -> GenProgram [L.Func a]
  -> MalgoM (L.Program a)
runGenProgram mainFunc ref m = do
  defs <- runReaderT m (ProgramEnv mempty ref)
  pure $ L.Program { L.functions = defs, mainFunc = mainFunc }

runGenExpr
  :: IDMap Type (ID LType)
  -> GenExpr (ID LType)
  -> GenProgram (Block (ID LType))
runGenExpr variableMap m = do
  psRef <- newIORef []
  value <- runReaderT m (ExprEnv psRef variableMap Nothing)
  ps    <- readIORef psRef
  pure $ Block { insts = reverse ps, value = value }

findVar :: ID Type -> GenExpr (ID LType)
findVar x = do
  ExprEnv { variableMap } <- ask
  pure $ fromJust $ lookup x variableMap

findFun :: ID Type -> GenProgram (ID LType)
findFun x = do
  ProgramEnv { functionMap } <- ask
  pure $ fromJust $ lookup x functionMap

-- LIR builder

addInnerFunc :: L.Func (ID LType) -> GenProgram ()
addInnerFunc fun = do
  ProgramEnv { innerFunctions } <- ask
  modifyIORef innerFunctions (fun :)

addInst :: Inst (ID LType) -> GenExpr (ID LType)
addInst inst = do
  ExprEnv { partialBlockInsts } <- ask
  i                             <- newID (ltypeOf inst) "%"
  -- traceShowM $ pPrint i <+> "=" <+> pPrint inst
  modifyIORef partialBlockInsts (\s -> (i, inst) : s)
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

binop :: L.Op -> ID LType -> ID LType -> GenExpr (ID LType)
binop op x y = addInst $ L.BinOp op x y

branchIf
  :: ID LType -> GenExpr (ID LType) -> GenExpr (ID LType) -> GenExpr (ID LType)
branchIf c genWhenTrue genWhenFalse = do
  tBlockRef <- newIORef []
  fBlockRef <- newIORef []
  tvalue    <- local (\s -> s { partialBlockInsts = tBlockRef }) genWhenTrue
  fvalue    <- local (\s -> s { partialBlockInsts = fBlockRef }) genWhenFalse
  tBlock    <- reverse <$> readIORef tBlockRef
  fBlock    <- reverse <$> readIORef fBlockRef

  addInst $ L.If c (Block tBlock tvalue) (Block fBlock fvalue)

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
convertType t = error $ toText $ "unreachable(convertType): " <> pShow t

-- generate LIR

genFunction :: M.Func Type (ID Type) -> GenProgram (L.Func (ID LType))
genFunction M.Func { name, captures = Nothing, params, body } = do
  funcName   <- findFun name
  funcParams <- forM params
    $ \ID { idName, idMeta } -> newID (convertType idMeta) idName
  bodyBlock <- runGenExpr (fromList (zip params funcParams)) (genExpr body)
  pure $ L.Func { name = funcName, params = funcParams, body = bodyBlock }
genFunction M.Func { name, captures = Just caps, mutrecs, params, body } = do
  funcName   <- findFun name
  capsId     <- newID (Ptr U8) "caps"
  funcParams <- mapM (\x -> newID (convertType (typeOf x)) (idName x)) params
  bodyBlock  <- runGenExpr (fromList (zip params funcParams)) $ do
    capsMap <- genUnpackCaps capsId
    clsMap  <- genCls capsId
    local
        (\s -> s { variableMap = variableMap s <> capsMap <> clsMap
                 , captures    = Just capsId
                 }
        )
      $ genExpr body
  pure $ L.Func { name   = funcName
                , params = capsId : funcParams
                , body   = bodyBlock
                }
 where
  genUnpackCaps capsId = do
    capsId' <- cast (Ptr $ Struct (map (convertType . typeOf) caps)) capsId
    foldForM (zip [0 ..] caps) $ \(i, c) -> do
      cOpr <- loadC capsId' [0, i]
      pure (one (c, cOpr))
  genCls capsId = foldForM mutrecs $ \f -> do
    f'    <- lift $ findFun f
    clsId <- packClosure f' capsId
    pure (one (f, clsId))

genMainFunction
  :: ID LType -> Expr Type (ID Type) -> GenProgram (L.Func (ID LType))
genMainFunction mainFuncId mainExpr = do
  body <- runGenExpr mempty $ genExpr mainExpr >> addInst (Constant $ Int32 0)
  pure $ L.Func { name = mainFuncId, params = [], body = body }

genExpr :: Expr Type (ID Type) -> GenExpr (ID LType)
genExpr (M.Var x             ) = findVar x
genExpr (M.Lit (Int    x    )) = addInst $ Constant $ Int64 $ fromInteger x
genExpr (M.Lit (Float  x    )) = addInst $ Constant $ Float64 x
genExpr (M.Lit (H.Bool True )) = addInst $ Constant $ L.Bool True
genExpr (M.Lit (H.Bool False)) = addInst $ Constant $ L.Bool False
genExpr (M.Lit (Char x)) = addInst $ Constant $ Word8 $ fromIntegral $ ord x
genExpr (M.Lit (H.String xs)) =
  addInst $ Constant $ L.String $ B.unpack $ encodeUtf8 @Text @ByteString xs
genExpr (M.Tuple xs) = do
  tuplePtr <- alloca (Struct $ map (convertType . typeOf) xs)
  forM_ (zip [0 ..] xs) $ \(i, x) -> do
    val <- findVar x
    storeC tuplePtr [0, i] val
  pure tuplePtr
genExpr (M.TupleAccess t i) = do
  tuplePtr <- findVar t
  loadC tuplePtr [0, i]
genExpr (M.MakeArray init size) = do
  initVal <- findVar init
  sizeVal <- findVar size
  arrayCreate initVal sizeVal
genExpr (M.ArrayRead arr ix) = do
  arrOpr <- findVar arr
  ixOpr  <- findVar ix
  load arrOpr ixOpr
genExpr (M.ArrayWrite arr ix val) = case typeOf arr of
  TyApp ArrayC [t] -> do
    arrOpr <- findVar arr
    ixOpr  <- findVar ix
    valOpr <- coerceTo (convertType t) =<< findVar val
    store arrOpr [ixOpr] valOpr
    undef (Ptr (Struct []))
  _ -> error $ toText $ pShow arr <> " is not an array"
genExpr (M.MakeClosure f cs) = do
  let capTy = Struct (map (convertType . typeOf) cs)
  capPtr <- alloca capTy
  forM_ (zip [0 ..] cs) $ \(i, c) -> do
    valOpr <- findVar c
    storeC capPtr [0, i] valOpr
  f'      <- lift $ findFun f
  capPtr' <- cast (Ptr U8) capPtr
  packClosure f' capPtr'
genExpr (M.CallDirect f args) = do
  funOpr  <- lift $ findFun f
  argOprs <- mapM findVar args
  call funOpr argOprs
genExpr (M.CallWithCaptures f args) = do
  funOpr                           <- lift $ findFun f
  ExprEnv { captures = Just caps } <- ask
  argOprs                          <- (caps :) <$> mapM findVar args
  call funOpr argOprs
genExpr (M.CallClosure f args) = do
  clsPtr  <- findVar f
  clsFun  <- loadC clsPtr [0, 0]
  clsCap  <- loadC clsPtr [0, 1]
  argOprs <- (clsCap :) <$> mapM findVar args
  call clsFun argOprs
genExpr (M.Let name val expr) = do
  val' <- coerceTo (convertType $ typeOf name) =<< genExpr val
  local (\st -> st { variableMap = insert name val' (variableMap st) })
    $ genExpr expr
genExpr (M.If c t f) = do
  cOpr <- findVar c
  branchIf cOpr (genExpr t) (genExpr f)
genExpr (M.Prim orig (TyApp FunC (r : ps)) xs) = do
  argOprs <- mapM findVar xs
  callExt orig (Function (convertType r) (map convertType ps)) argOprs
genExpr M.Prim{}         = error "external variable is not supported"
genExpr (M.BinOp op x y) = do
  xOpr <- findVar x
  yOpr <- findVar y
  binop op' xOpr yOpr
 where
  op' = case (op, convertType $ typeOf x) of
    (Add , _  ) -> ADD
    (Sub , _  ) -> SUB
    (Mul , _  ) -> MUL
    (Div , _  ) -> SDIV
    (Mod , _  ) -> SREM
    (FAdd, _  ) -> FADD
    (FSub, _  ) -> FSUB
    (FMul, _  ) -> FMUL
    (FDiv, _  ) -> FDIV
    (Eq  , Bit) -> IEQ
    (Neq , Bit) -> INE
    (Eq  , U8 ) -> IEQ
    (Neq , U8 ) -> INE
    (Eq  , I64) -> IEQ
    (Eq  , F64) -> FEQ
    (Neq , I64) -> INE
    (Neq , F64) -> FNE
    (Lt  , I64) -> SLT
    (Le  , I64) -> SLE
    (Gt  , I64) -> SGT
    (Ge  , I64) -> SGE
    (Lt  , F64) -> FLT
    (Le  , F64) -> FLE
    (Gt  , F64) -> FGT
    (Ge  , F64) -> FGE
    (And , _  ) -> AND
    (Or  , _  ) -> OR
    (_   , t  ) -> error $ show t <> " is not comparable"

packClosure :: ID LType -> ID LType -> ReaderT ExprEnv GenProgram (ID LType)
packClosure f capsId = do
  clsId <- alloca (Struct [ltypeOf f, Ptr U8])
  storeC clsId [0, 0] f
  storeC clsId [0, 1] capsId
  pure clsId

-- | convert to boxed value
wrap :: LType -> ID LType -> GenExpr (ID LType)
wrap (Ptr U8) = pure
wrap (Ptr (Struct [Function r (Ptr U8 : ps), Ptr U8])) = \x -> do
  -- generate new captured environment
  -- fw captures the original closure
  -- note: ltypeOf x == Ptr $ Struct [Function r (Ptr U8:ps), Ptr U8]
  boxedX <- cast (Ptr U8) x

  -- generate fw
  fwName <- newID
    (Function (Ptr U8) (Ptr U8 : replicate (length ps) (Ptr U8)))
    "fw"
  fwBoxedXName <- newID (Ptr U8) "boxedCaps"
  fwParamNames <- replicateM (length ps) $ newID (Ptr U8) "a"
  bodyBlock    <- lift $ runGenExpr mempty $ do
    fwUnboxedX <- cast (ltypeOf x) fwBoxedXName
    as         <- zipWithM unwrap ps fwParamNames
    xFun       <- loadC fwUnboxedX [0, 0]
    xCap       <- loadC fwUnboxedX [0, 1]
    retVal     <- call xFun (xCap : as)
    wrap r retVal
  lift $ addInnerFunc $ L.Func { name   = fwName
                               , params = fwBoxedXName : fwParamNames
                               , body   = bodyBlock
                               }
  cast (Ptr U8) =<< packClosure fwName boxedX
wrap (Ptr (Struct ts)) = \x -> do
  ptr <- alloca (Struct (replicate (length ts) (Ptr U8)))
  forM_ (zip [0 ..] ts) $ \(i, t) -> do
    raw    <- loadC x [0, i]
    wraped <- wrap t raw
    storeC ptr [0, i] wraped
  cast (Ptr U8) ptr
wrap (Ptr _)    = cast (Ptr U8)
wrap Bit        = zext U64 >=> cast (Ptr U8)
wrap I32        = sext I64 >=> cast (Ptr U8)
wrap I64        = cast (Ptr U8)
wrap U8         = zext U64 >=> cast (Ptr U8)
wrap U32        = zext U64 >=> cast (Ptr U8)
wrap U64        = cast (Ptr U8)
wrap F64        = cast (Ptr U8)
wrap Function{} = cast (Ptr U8)
wrap Struct{}   = error "cannot convert Struct to boxed value"
wrap Void       = error "cannot convert Void to boxed value"

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
  foParamNames <- mapM (\t -> newID t "a") ps
  bodyBlock    <- lift $ runGenExpr mempty $ do
    foUnboxedX <- cast
      (Ptr
        (Struct
          [Function (Ptr U8) (Ptr U8 : replicate (length ps) (Ptr U8)), Ptr U8]
        )
      )
      foXName
    wrappedParams <- mapM (\p -> wrap (ltypeOf p) p) foParamNames
    xFun          <- loadC foUnboxedX [0, 0]
    xCap          <- loadC foUnboxedX [0, 1]
    retVal        <- call xFun (xCap : wrappedParams)
    unwrap r retVal
  lift $ addInnerFunc $ L.Func { name   = foName
                               , params = foXName : foParamNames
                               , body   = bodyBlock
                               }
  packClosure foName x
unwrap (Ptr (Struct ts)) = \x -> do
  x' <- cast (Ptr $ Struct $ replicate (length ts) (Ptr U8)) x
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
coerceTo (Ptr U8) x           = wrap (ltypeOf x) x
coerceTo (Ptr (Struct [Function r (Ptr U8 : ps), Ptr U8])) x =
  case ltypeOf x of
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
      lift $ addInnerFunc $ L.Func { name   = fName
                                   , params = fBoxedXName : fParamNames
                                   , body   = bodyBlock
                                   }
      packClosure fName boxedX
    _ -> error "x is not closure"
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
