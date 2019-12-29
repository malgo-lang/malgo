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
    mainFuncId <- newID (Function I32 []) "main"
    funMap     <- foldMapM genFunMap functions
    runGenProgram mainFuncId $ local (\s -> s { functionMap = funMap }) $ do
      fs <- mapM genFunction functions
      mf <- genMainFunction mainFuncId mainExpr
      pure (mf : fs)
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

newtype ProgramEnv = ProgramEnv { functionMap :: IDMap Type (ID LType) }

data ExprEnv = ExprEnv { partialBlockInsts :: IORef [(ID LType, Inst (ID LType))]
                       , variableMap       :: IDMap Type (ID LType)
                       , captures          :: Maybe (ID LType) }

type GenProgram = ReaderT ProgramEnv MalgoM
type GenExpr = ReaderT ExprEnv GenProgram

runGenProgram :: a -> GenProgram [L.Func a] -> MalgoM (L.Program a)
runGenProgram mainFunc m = do
  defs <- runReaderT m (ProgramEnv mempty)
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

addInst :: Inst (ID LType) -> GenExpr (ID LType)
addInst inst = do
  ExprEnv { partialBlockInsts } <- ask
  i                             <- newID (ltypeOf inst) "%"
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
    as <- zipWithM genArg ps xs
    addInst $ Call f as
  _ -> error "function must be typed as function"

callExt :: Text -> LType -> [ID LType] -> GenExpr (ID LType)
callExt f funTy xs = case funTy of
  Function _ ps -> do
    as <- zipWithM genArg ps xs
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
    clsId <- packClosure f capsId
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
genExpr (M.ArrayWrite arr ix val) =
  case typeOf arr of
    TyApp ArrayC [t] -> do
      arrOpr <- findVar arr
      ixOpr  <- findVar ix
      valOpr <- genArg (convertType t) =<< findVar val
      store arrOpr [ixOpr] valOpr
      undef (Ptr (Struct []))
    _ -> error $ toText $ pShow arr <> " is not an array"
genExpr (M.MakeClosure f cs) = do
  let capTy = Struct (map (convertType . typeOf) cs)
  capPtr <- alloca capTy
  forM_ (zip [0 ..] cs) $ \(i, c) -> do
    valOpr <- findVar c
    storeC capPtr [0, i] valOpr
  packClosure f =<< cast (Ptr U8) capPtr
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
  val' <- genExpr val
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

packClosure :: ID Type -> ID LType -> ReaderT ExprEnv GenProgram (ID LType)
packClosure f capsId = case convertType $ typeOf f of
  Ptr clsTy -> do
    clsId <- alloca clsTy
    storeC clsId [0, 0] =<< lift (findFun f)
    storeC clsId [0, 1] capsId
    pure clsId
  _ -> error "packClosure"

-- | convert to boxed value
wrap :: LType -> ID LType -> GenExpr (ID LType)
wrap (Ptr U8)    = pure
wrap (Ptr _ )    = cast (Ptr U8)
wrap Bit         = zext U64 >=> cast (Ptr U8)
wrap I32         = sext I64 >=> cast (Ptr U8)
wrap I64         = cast (Ptr U8)
wrap U8          = zext U64 >=> cast (Ptr U8)
wrap U32         = zext U64 >=> cast (Ptr U8)
wrap U64         = cast (Ptr U8)
wrap F64         = cast (Ptr U8)
wrap (Struct ts) = \x -> do
  ptr <- alloca (Struct (replicate (length ts) (Ptr U8)))
  forM_ (zip [0 ..] ts) $ \(i, t) -> do
    raw    <- loadC x [i]
    wraped <- wrap t raw
    storeC ptr [0, i] wraped
  pure ptr
wrap Function{} = cast (Ptr U8)
wrap Void       = error "cannot convert Void to boxed value"

-- | convert boxed value to unboxed value
unwrap :: LType -> ID LType -> GenExpr (ID LType)
unwrap (Ptr U8)    = pure
unwrap (Ptr t )    = cast (Ptr t)
unwrap Bit         = cast U64 >=> trunc Bit
unwrap I32         = cast I64 >=> trunc I32
unwrap I64         = cast I64
unwrap U8          = cast U64 >=> trunc U8
unwrap U32         = cast U64 >=> trunc U32
unwrap U64         = cast U64
unwrap F64         = cast F64
unwrap (Struct ts) = \x -> do
  ptr <- alloca (Struct ts)
  forM_ (zip [0 ..] ts) $ \(i, t) -> do
    wraped <- loadC x [0, i]
    raw    <- unwrap t wraped
    storeC ptr [0, i] raw
  loadC ptr [0]
unwrap t@Function{} = cast t
unwrap Void       = error "cannot convert boxed value to Void"

genArg :: HasCallStack => LType -> ID LType -> GenExpr (ID LType)
genArg t x | t == ltypeOf x = pure x
genArg (Ptr    U8) x        = wrap (ltypeOf x) x
genArg (Struct ts) x        = do
  ptr <- alloca (Struct ts)
  forM_ (zip [0 ..] ts) $ \(i, t) -> do
    xElem  <- loadC x [0]
    xElem' <- genArg t xElem
    storeC ptr [0, i] xElem'
  loadC ptr [0]
genArg t x = case ltypeOf x of
  Ptr U8 -> unwrap t x
  _      -> error $ toText $ "cannot convert " <> pShow x <> "\n to " <> pShow t
