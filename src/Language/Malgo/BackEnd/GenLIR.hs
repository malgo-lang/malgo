{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Malgo.BackEnd.GenLIR
  ( GenLIR
  )
where

import           Language.Malgo.ID
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Prelude
import           Language.Malgo.Pretty

import           Language.Malgo.IR.HIR         as H
import           Language.Malgo.IR.LIR         as L
import           Language.Malgo.IR.MIR         as M

import           Language.Malgo.TypeRep.LType  as L
import           Language.Malgo.TypeRep.Type   as M

import           Language.Malgo.BackEnd.LIRBuilder
                                               as L

import           Control.Lens                   ( (.~)
                                                , ifoldMap
                                                , ifor_
                                                )
import           Relude.Unsafe                  ( fromJust )

data GenLIR

instance Pass GenLIR (M.Program (ID Type)) (L.Program (ID LType)) where
  passName = "GenLIR"
  isDump   = dumpLIR
  trans M.Program { functions, mainExpr } =
    runProgramBuilderT
        (foldMap
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
          runExprBuilderT (ExprEnv mempty Nothing) $ do
            _ <- callExt "GC_init" (Function Void []) []
            genExpr mainExpr

-- generate LIR
genFunction :: (MonadUniq m, MonadMalgo m, MonadProgramBuilder m) => M.Func (ID Type) -> m ()
genFunction M.Func { name, captures = Nothing, params, body } = do
  funcName <- findFunc name
  let funcParams = map (\p@ID { idMeta } -> p & metaL .~ convertType idMeta) params
  bodyBlock <- runExprBuilderT (ExprEnv (fromList (zip params funcParams)) Nothing) (genExpr body)
  addFunc $ L.Func { name = funcName, params = funcParams, body = bodyBlock }
genFunction M.Func { name, captures = Just caps, mutrecs, params, body } = do
  funcName <- findFunc name
  capsId   <- newID (Ptr U8) "$caps"
  let funcParams = map (\x -> x & metaL .~ convertType (typeOf x)) params
  bodyBlock <- runExprBuilderT (ExprEnv (fromList (zip params funcParams)) (Just capsId)) $ do
    -- unwrap captures
    unwrapedCapsId <- cast (Ptr $ Struct (map (convertType . typeOf) caps)) capsId
    capsMap        <- getAp
      $ ifoldMap (\i -> Ap . fmap one . traverseToSnd (const $ loadC unwrapedCapsId [0, i])) caps
    -- generate closures of mutrec functions
    clsMap <- foldMapA (fmap one . traverseToSnd (findFunc >=> packClosure capsId)) mutrecs
    withVariables (capsMap <> clsMap) $ genExpr body
  addFunc $ L.Func { name = funcName, params = capsId : funcParams, body = bodyBlock }

genExpr :: (MonadUniq m, MonadExprBuilder m) => M.Expr (ID Type) -> m (ID LType)
genExpr (M.Var   x               ) = findVar x
genExpr (M.Lit   (Int      x    )) = assign $ Constant $ Int64 $ fromInteger x
genExpr (M.Lit   (Float    x    )) = assign $ Constant $ Float64 x
genExpr (M.Lit   (H.Bool   True )) = assign $ Constant $ L.Bool True
genExpr (M.Lit   (H.Bool   False)) = assign $ Constant $ L.Bool False
genExpr (M.Lit   (Char     x    )) = assign $ Constant $ Word8 $ fromIntegral $ ord x
genExpr (M.Lit   (H.String xs   )) = assign $ Constant $ L.String xs
genExpr (M.Tuple xs              ) = do
  tuplePtr <- alloca (Struct $ map (convertType . typeOf) xs)
  ifor_ xs $ \i x -> storeC tuplePtr [0, i] =<< findVar x
  pure tuplePtr
genExpr (M.TupleAccess t i) = do
  tuplePtr <- findVar t
  loadC tuplePtr [0, i]
genExpr (M.MakeArray init size) = do
  initVal <- findVar init
  sizeVal <- coerceTo SizeT =<< findVar size
  ptr     <- arrayCreate (ltypeOf initVal) sizeVal
  zero    <- assign $ Constant $ Int64 0
  forLoop zero sizeVal $ \idx -> do
    rawAddr <- loadC ptr [0, 0]
    store rawAddr [idx] initVal
  pure ptr
genExpr (M.ArrayRead arr idx) = do
  arrRawOpr <- flip loadC [0, 0] =<< findVar arr
  ixOpr     <- coerceTo SizeT =<< findVar idx
  case ltypeOf arrRawOpr of
    Ptr t -> load t arrRawOpr [ixOpr]
    _     -> error $ toText $ pShow arr <> " is not an array"
genExpr (M.ArrayWrite arr idx val) = case typeOf arr of
  TyApp ArrayC [t] -> do
    arrRawOpr <- flip loadC [0, 0] =<< findVar arr
    ixOpr     <- coerceTo SizeT =<< findVar idx
    valOpr    <- coerceTo (convertType t) =<< findVar val
    store arrRawOpr [ixOpr] valOpr
    undef (Ptr (Struct [])) -- TODO: generate unique Unit value
  _ -> error $ toText $ pShow arr <> " is not an array"
genExpr (M.MakeClosure f cs) = do
  capPtr <- alloca $ Struct (map (convertType . typeOf) cs)
  ifor_ cs $ \i c -> do
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
