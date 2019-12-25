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

import           Control.Exception              ( assert )
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
                       , nameHint          :: Text
                       , captures          :: Maybe (ID LType) }

type GenProgram = ReaderT ProgramEnv MalgoM
type GenExpr = ReaderT ExprEnv GenProgram

runGenProgram :: a -> GenProgram [L.Func a] -> MalgoM (L.Program a)
runGenProgram mainFunc m = do
  defs <- runReaderT m (ProgramEnv mempty)
  pure $ L.Program { L.functions = defs, mainFunc = mainFunc }

runGenExpr
  :: IDMap Type (ID LType)
  -> Text
  -> GenExpr (ID LType)
  -> GenProgram (Block (ID LType))
runGenExpr variableMap nameHint m = do
  psRef <- newIORef []
  value <- runReaderT m (ExprEnv psRef variableMap nameHint Nothing)
  ps    <- readIORef psRef
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
  pure $ fromJust $ lookup x variableMap

findFun :: ID Type -> GenProgram (ID LType)
findFun x = do
  ProgramEnv { functionMap } <- ask
  pure $ fromJust $ lookup x functionMap

setHint :: MonadReader ExprEnv m => Text -> m a -> m a
setHint x = local (\s -> s { nameHint = x })

alloca :: LType -> Maybe (ID LType) -> GenExpr (ID LType)
alloca ty msize = addInst $ Alloca ty msize

loadC :: ID LType -> [Int] -> GenExpr (ID LType)
loadC ptr xs = addInst $ LoadC ptr xs

load :: ID LType -> ID LType -> GenExpr (ID LType)
load ptr xs = addInst $ Load ptr xs

storeC :: ID LType -> [Int] -> ID LType -> GenExpr ()
storeC ptr xs val = addInst (StoreC ptr xs val) >> pass

store :: ID LType -> [ID LType] -> ID LType -> GenExpr ()
store ptr xs val = addInst (Store ptr xs val) >> pass

call
  :: HasCallStack
  => ID LType
  -> [ID LType]
  -> ReaderT ExprEnv GenProgram (ID LType)
call f xs = do
  case (ltypeOf f, map ltypeOf xs) of
    (Function _ ps, as) -> assert (ps == as) pass
    _                   -> error "function must be typed as function"
  addInst $ Call f xs
callExt :: Text -> LType -> [ID LType] -> ReaderT ExprEnv GenProgram (ID LType)
callExt f funTy xs = do
  case (funTy, map ltypeOf xs) of
    (Function _ ps, as) -> assert (ps == as) pass
    _                   -> error "external function must be typed as function"
  addInst $ CallExt f funTy xs

cast :: LType -> ID LType -> GenExpr (ID LType)
cast ty val = addInst $ Cast ty val

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
convertType t = error $ toText $ "unreachable(convertType): " <> pShow t

genFunction :: M.Func Type (ID Type) -> GenProgram (L.Func (ID LType))
genFunction M.Func { name, captures = Nothing, params, body } = do
  funcName   <- findFun name
  funcParams <- forM params
    $ \ID { idName, idMeta } -> newID (convertType idMeta) idName
  bodyBlock <- runGenExpr (fromList (zip params funcParams)) "x" (genExpr body)
  pure $ L.Func { name = funcName, params = funcParams, body = bodyBlock }
genFunction M.Func { name, captures = Just caps, mutrecs, params, body } = do
  funcName   <- findFun name
  capsId     <- newID (Ptr U8) "caps"
  funcParams <- mapM (\x -> newID (convertType (typeOf x)) (idName x)) params
  bodyBlock  <- runGenExpr (fromList (zip params funcParams)) "x" $ do
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
    capsId' <- setHint "boxedCaps"
      $ cast (Ptr $ Struct (map (convertType . typeOf) caps)) capsId
    setHint "capture" $ foldForM (zip [0 ..] caps) $ \(i, c) -> do
      cOpr <- loadC capsId' [0, i]
      pure (one (c, cOpr))
  genCls capsId = foldForM mutrecs $ \f -> do
    clsId <- setHint (idName f) $ packClosure f capsId
    pure (one (f, clsId))

genMainFunction
  :: ID LType -> Expr Type (ID Type) -> GenProgram (L.Func (ID LType))
genMainFunction mainFuncId mainExpr = do
  body <- runGenExpr mempty "x" $ genExpr mainExpr >> addInst
    (Constant $ Int32 0)
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
  tuplePtr <- alloca (Struct $ map (convertType . typeOf) xs) Nothing
  forM_ (zip [0 ..] xs) $ \(i, x) -> do
    val <- findVar x
    storeC tuplePtr [0, i] val
  pure tuplePtr
genExpr (M.TupleAccess t i) = do
  tuplePtr <- findVar t
  loadC tuplePtr [0, i]
genExpr (M.MakeArray ty size) = do
  sizeVal <- findVar size
  alloca (convertType ty) (Just sizeVal)
genExpr (M.ArrayRead arr ix) = do
  arrOpr <- findVar arr
  ixOpr  <- findVar ix
  load arrOpr ixOpr
genExpr (M.ArrayWrite arr ix val) = do
  arrOpr <- findVar arr
  ixOpr  <- findVar ix
  valOpr <- findVar val
  store arrOpr [ixOpr] valOpr
  undef (Ptr (Struct []))
genExpr (M.MakeClosure f cs) = do
  let capTy = Struct (map (convertType . typeOf) cs)
  capPtr <- setHint "captures" $ alloca capTy Nothing
  setHint "capture" $ forM_ (zip [0 ..] cs) $ \(i, c) -> do
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
  clsFun  <- setHint "clsFun" $ loadC clsPtr [0, 0]
  clsCap  <- setHint "clsCap" $ loadC clsPtr [0, 1]
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
packClosure f capsId = setHint "closure" $ case convertType $ typeOf f of
  Ptr clsTy -> do
    clsId <- alloca clsTy Nothing
    storeC clsId [0, 0] =<< lift (findFun f)
    storeC clsId [0, 1] capsId
    pure clsId
  _ -> error "packClosure"
