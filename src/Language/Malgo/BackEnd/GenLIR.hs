{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.BackEnd.GenLIR where

import           Control.Lens
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
import           Language.Malgo.BackEnd.LIRBuilder
                                               as L

data GenLIR

instance Pass GenLIR (M.Program Type (ID Type)) (L.Program (ID LType)) where
  passName = "GenLIR"
  isDump = dumpLIR
  trans M.Program { functions, mainExpr } = do
    logDebug "Start GenLIR"
    funMap <- foldMapM genFunMap functions
    runGenProgram $ local (set functionMap funMap) $ do
      mapM_ (addFunc <=< genFunction) functions
      runGenExpr mempty $ genExpr mainExpr
   where
    genFunMap M.Func { name, captures, params, body } = do
      let ps = map typeOf params
          r  = typeOf body
      let newName = updateID name (functionType (isNothing captures) ps r)
      pure $ one (name, newName)
    functionType isKnown ps r
      | isKnown   = Function (convertType $ typeOf r) (map (convertType . typeOf) ps)
      | otherwise = Function (convertType $ typeOf r) (Ptr U8 : map (convertType . typeOf) ps)

-- generate LIR

genFunction :: M.Func Type (ID Type) -> GenProgram (L.Func (ID LType))
genFunction M.Func { name, captures = Nothing, params, body } = do
  funcName   <- findFun name
  let funcParams = map (\p@ID{ idMeta } -> updateID p (convertType idMeta)) params
  bodyBlock  <- runGenExpr (fromList (zip params funcParams)) (genExpr body)
  pure $ L.Func { name = funcName, params = funcParams, body = bodyBlock }
genFunction M.Func { name, captures = Just caps, mutrecs, params, body } = do
  funcName   <- findFun name
  capsId     <- newID (Ptr U8) "caps"
  let funcParams = map (\x -> updateID x (convertType (typeOf x))) params
  bodyBlock  <- runGenExpr (fromList (zip params funcParams)) $ do
    capsMap <- genUnpackCaps capsId
    clsMap  <- genCls capsId
    local (over variableMap ((capsMap <> clsMap) <>) . set currentCaptures (Just capsId))
      $ genExpr body
  pure $ L.Func { name = funcName, params = capsId : funcParams, body = bodyBlock }
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

genMainFunction :: ID LType -> Expr Type (ID Type) -> GenProgram (L.Func (ID LType))
genMainFunction mainFuncId mainExpr = do
  body <- runGenExpr mempty $ genExpr mainExpr >> addInst (Constant $ Int32 0)
  pure $ L.Func { name = mainFuncId, params = [], body = body }

genExpr :: Expr Type (ID Type) -> GenExpr (ID LType)
genExpr (M.Var x             ) = findVar x
genExpr (M.Lit (Int    x    )) = addInst $ Constant $ Int64 $ fromInteger x
genExpr (M.Lit (Float  x    )) = addInst $ Constant $ Float64 x
genExpr (M.Lit (H.Bool True )) = addInst $ Constant $ L.Bool True
genExpr (M.Lit (H.Bool False)) = addInst $ Constant $ L.Bool False
genExpr (M.Lit (Char   x    )) = addInst $ Constant $ Word8 $ fromIntegral $ ord x
genExpr (M.Lit (H.String xs)) =
  addInst $ Constant $ L.String xs
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
  sizeVal <- coerceTo SizeT =<< findVar size
  ptr     <- arrayCreate (ltypeOf initVal) sizeVal
  zero    <- addInst $ Constant $ Int64 0
  _       <- forLoop zero sizeVal $ \idx -> do
    rawAddr <- loadC ptr [0, 0]
    store rawAddr [idx] initVal
  pure ptr
genExpr (M.ArrayRead arr idx) = do
  arrOpr <- findVar arr
  arrRawOpr <- loadC arrOpr [0, 0]
  ixOpr  <- coerceTo SizeT =<< findVar idx
  load arrRawOpr ixOpr
genExpr (M.ArrayWrite arr idx val) = case typeOf arr of
  TyApp ArrayC [t] -> do
    arrOpr <- findVar arr
    arrRawOpr <- loadC arrOpr [0, 0]
    ixOpr  <- coerceTo SizeT =<< findVar idx
    valOpr <- coerceTo (convertType t) =<< findVar val
    _ <- store arrRawOpr [ixOpr] valOpr
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
  funOpr    <- lift $ findFun f
  Just caps <- view currentCaptures
  argOprs   <- (caps :) <$> mapM findVar args
  call funOpr argOprs
genExpr (M.CallClosure f args) = do
  clsPtr  <- findVar f
  clsFun  <- loadC clsPtr [0, 0]
  clsCap  <- loadC clsPtr [0, 1]
  argOprs <- (clsCap :) <$> mapM findVar args
  call clsFun argOprs
genExpr (M.Let name val expr) = do
  val' <- coerceTo (convertType $ typeOf name) =<< genExpr val
  local (over variableMap (insert name val')) $ genExpr expr
genExpr (M.If c t f) = do
  cOpr <- findVar c
  branchIf cOpr (genExpr t) (genExpr f)
genExpr (M.Prim orig (TyApp FunC (r : ps)) xs) = do
  argOprs <- mapM findVar xs
  callExt orig (Function (convertType r) (map convertType ps)) argOprs
genExpr M.Prim{}        = error "external variable is not supported"
genExpr (M.BinOp o x y) = do
  xOpr <- findVar x
  yOpr <- findVar y
  binop o' xOpr yOpr
 where
  o' = case (o, convertType $ typeOf x) of
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
