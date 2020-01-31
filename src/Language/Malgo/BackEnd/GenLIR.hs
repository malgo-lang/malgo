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
import           Relude.Unsafe                  ( fromJust )

data GenLIR

instance Pass GenLIR (M.Program (ID Type)) (L.Program (ID LType)) where
  passName = "GenLIR"
  isDump   = dumpLIR
  trans M.Program { functions, mainExpr } = do
    logDebug "Start GenLIR"
    runProgramBuilder
        (ProgramEnv $ foldMap
          (\M.Func { name, captures, params, body } -> one
            ( name
            , name & metaL .~ Function
              (convertType $ typeOf body)
              (if isNothing captures
                then map (convertType . typeOf) params
                else Ptr U8 : map (convertType . typeOf) params
              )
            )
          )
          functions
        )
      $ do
          mapM_ genFunction functions
          runExprBuilder (ExprEnv mempty Nothing) $ genExpr mainExpr

-- generate LIR
genFunction :: M.Func (ID Type) -> ProgramBuilder ()
genFunction M.Func { name, captures = Nothing, params, body } = do
  funcName <- findFunc name
  let funcParams = map (\p@ID { idMeta } -> p & metaL .~ convertType idMeta) params
  bodyBlock <- runExprBuilder (ExprEnv (fromList (zip params funcParams)) Nothing) (genExpr body)
  addFunc $ L.Func { name = funcName, params = funcParams, body = bodyBlock }
genFunction M.Func { name, captures = Just caps, mutrecs, params, body } = do
  funcName <- findFunc name
  capsId   <- newID (Ptr U8) "$caps"
  let funcParams = map (\x -> x & metaL .~ convertType (typeOf x)) params
  bodyBlock <- runExprBuilder (ExprEnv (fromList (zip params funcParams)) (Just capsId)) $ do
    -- unwrap captures
    unwrapedCapsId <- cast (Ptr $ Struct (map (convertType . typeOf) caps)) capsId
    capsMap <- ifoldMap (\i -> fmap one . traverseToSnd (const $ loadC unwrapedCapsId [0, i])) caps
    -- generate closures of mutrec functions
    clsMap <- foldMap (fmap one . traverseToSnd (findFunc >=> packClosure capsId)) mutrecs
    withVariables (capsMap <> clsMap) $ genExpr body
  addFunc $ L.Func { name = funcName, params = capsId : funcParams, body = bodyBlock }

genMainFunction :: ID LType -> Expr (ID Type) -> ProgramBuilder (L.Func (ID LType))
genMainFunction mainFuncId mainExpr = do
  body <- runExprBuilder (ExprEnv mempty Nothing) $ genExpr mainExpr >> addInst (Constant $ Int32 0)
  pure $ L.Func { name = mainFuncId, params = [], body = body }

genExpr :: MonadExprBuilder m => Expr (ID Type) -> m (ID LType)
genExpr (M.Var   x               ) = findVar x
genExpr (M.Lit   (Int      x    )) = addInst $ Constant $ Int64 $ fromInteger x
genExpr (M.Lit   (Float    x    )) = addInst $ Constant $ Float64 x
genExpr (M.Lit   (H.Bool   True )) = addInst $ Constant $ L.Bool True
genExpr (M.Lit   (H.Bool   False)) = addInst $ Constant $ L.Bool False
genExpr (M.Lit   (Char     x    )) = addInst $ Constant $ Word8 $ fromIntegral $ ord x
genExpr (M.Lit   (H.String xs   )) = addInst $ Constant $ L.String xs
genExpr (M.Tuple xs              ) = do
  tuplePtr <- alloca (Struct $ map (convertType . typeOf) xs)
  iforM_ xs $ \i x -> storeC tuplePtr [0, i] =<< findVar x
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
  arrOpr    <- findVar arr
  arrRawOpr <- loadC arrOpr [0, 0]
  ixOpr     <- coerceTo SizeT =<< findVar idx
  load arrRawOpr ixOpr
genExpr (M.ArrayWrite arr idx val) = case typeOf arr of
  TyApp ArrayC [t] -> do
    arrOpr    <- findVar arr
    arrRawOpr <- loadC arrOpr [0, 0]
    ixOpr     <- coerceTo SizeT =<< findVar idx
    valOpr    <- coerceTo (convertType t) =<< findVar val
    _         <- store arrRawOpr [ixOpr] valOpr
    undef (Ptr (Struct []))
  _ -> error $ toText $ pShow arr <> " is not an array"
genExpr (M.MakeClosure f cs) = do
  let capTy = Struct (map (convertType . typeOf) cs)
  capPtr <- alloca capTy
  iforM_ cs $ \i c -> do
    valOpr <- findVar c
    storeC capPtr [0, i] valOpr
  f'      <- findFunc f
  capPtr' <- cast (Ptr U8) capPtr
  packClosure capPtr' f'
genExpr (M.CallDirect f args) = do
  funOpr  <- findFunc f
  argOprs <- mapM findVar args
  call funOpr argOprs
genExpr (M.CallWithCaptures f args) = do
  funOpr  <- findFunc f
  caps    <- fromJust <$> getCurrentCaptures
  argOprs <- (caps :) <$> mapM findVar args
  call funOpr argOprs
genExpr (M.CallClosure f args) = do
  clsPtr  <- findVar f
  clsFun  <- loadC clsPtr [0, 0]
  clsCap  <- loadC clsPtr [0, 1]
  argOprs <- (clsCap :) <$> mapM findVar args
  call clsFun argOprs
genExpr (M.Let name val expr) = do
  val' <- coerceTo (convertType $ typeOf name) =<< genExpr val
  withVariables (fromList [(name, val')]) $ genExpr expr
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
