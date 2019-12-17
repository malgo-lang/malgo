{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
module Language.Malgo.BackEnd.GenLIR where

import qualified Data.ByteString                   as B
import           Language.Malgo.BackEnd.LIRBuilder
import           Language.Malgo.ID
import           Language.Malgo.IR.HIR             as H (Lit (..), Op (..))
import           Language.Malgo.IR.LIR             as L
import           Language.Malgo.IR.MIR             as M
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Pretty
import           Language.Malgo.TypeRep.LType      as L
import           Language.Malgo.TypeRep.Type       as M
import           Relude                            hiding (Type)
import           Relude.Extra.Map                  hiding (size)

data GenLIR

instance Pass GenLIR (M.Program Type (ID Type)) (L.Program (ID LType)) where
  isDump _ = False
  trans (M.Program { functions, mainExpr }) = do
    mainFuncId <- newID (Function I32 []) "main"
    runGenProgram mainFuncId $ do
      fs <- mapM genFunction functions
      mf <- genMainFunction mainFuncId mainExpr
      pure (mf : fs)

genFunction M.Func{ name, captures, mutrecs, params, body } = undefined

genMainFunction mainFuncId mainExpr = do
  body <- runGenExpr mempty "x" $ genExpr mainExpr
  pure $ L.Func { name = mainFuncId, params = [], body = body }

genExpr :: (MonadMalgo (t m), MonadTrans t,
              MonadReader ExprEnv (t m), MonadReader ProgramEnv m,
              MonadFail (t m)) =>
             Expr Type (ID Type) -> t m (ID LType)
genExpr (M.Var x)       = addInst =<< L.Var <$> findVar x
genExpr (M.Lit (Int x)) = addInst $ Constant $ Int64 $ fromInteger x
genExpr (M.Lit (Float x)) = addInst $ Constant $ Float64 x
genExpr (M.Lit (H.Bool True)) = addInst $ Constant $ L.Bool True
genExpr (M.Lit (H.Bool False)) = addInst $ Constant $ L.Bool False
genExpr (M.Lit (Char x)) = addInst $ Constant $ Word8 $ fromIntegral $ ord x
genExpr (M.Lit (H.String xs)) = addInst $ Constant $ L.String $ B.unpack $ encodeUtf8 @Text @ByteString xs
genExpr (M.Tuple xs) = do
  tuplePtr <- alloca (Struct $ map (convertType . typeOf) xs) Nothing
  forM_ (zip [0..] xs) $ \(i, x) -> do
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
  ixOpr <- findVar ix
  load arrOpr [ixOpr]
genExpr (M.ArrayWrite arr ix val) = do
  arrOpr <- findVar arr
  ixOpr <- findVar ix
  valOpr <- findVar val
  _ <- store arrOpr [ixOpr] valOpr
  undef (Ptr (Struct []))
genExpr (M.MakeClosure f cs) = do
  let capTy = Struct (map (convertType . typeOf) cs)
  capPtr <- alloca capTy Nothing
  forM_ (zip [0..] cs) $ \(i, c) -> do
    valOpr <- findVar c
    storeC capPtr [0, i] valOpr
  packClosure f =<< cast Boxed capPtr
genExpr (M.CallDirect f args) = do
  funOpr <- lift $ findFun f
  argOprs <- mapM findVar args
  call funOpr argOprs
genExpr (M.CallWithCaptures f args) = do
  funOpr <- lift $ findFun f
  ExprEnv { captures = Just caps } <- ask
  argOprs <- (caps : ) <$> mapM findVar args
  call funOpr argOprs
genExpr (M.CallClosure f args) = do
  clsPtr <- findVar f
  clsFun <- loadC clsPtr [0, 0]
  clsCap <- loadC clsPtr [0, 1]
  argOprs <- (clsCap :) <$> mapM findVar args
  call clsFun argOprs
genExpr (M.Let defs e) = do
  defsMap <- mapM (\(x, val) -> (x, ) <$> genExpr val) defs
  local (\st -> st { variableMap = fromList defsMap <> variableMap st }) $ genExpr e
genExpr (M.If c t f) = do
  cOpr <- findVar c
  branchIf cOpr (genExpr t) (genExpr f)
genExpr (M.Prim orig (TyFun _ ty) xs) = do
  argOprs <- mapM findVar xs
  callExt orig (convertType ty) argOprs
genExpr M.Prim{} = error "extern symbol must have a function type"
genExpr (M.BinOp op x y) = do
  xOpr <- findVar x
  yOpr <- findVar y
  binop op' xOpr yOpr
  where op' = case (op, convertType $ typeOf x) of
          (Add, _) -> ADD; (Sub, _) -> SUB; (Mul, _) -> MUL; (Div, _) -> SDIV;
          (Mod, _) -> SREM;
          (FAdd, _) -> FADD; (FSub, _) -> FSUB; (FMul, _) -> FMUL; (FDiv, _) -> FDIV;
          (Eq, I64) -> IEQ; (Eq, F64) -> FEQ; (Neq, I64) -> INE; (Neq, F64) -> FNE;
          (Lt, I64) -> SLT; (Le, I64) -> SLE; (Gt, I64) -> SGT; (Ge, I64) -> SGE;
          (Lt, F64) -> FLT; (Le, F64) -> FLE; (Gt, F64) -> FGT; (Ge, F64) -> FGE;
          (And, _) -> AND; (Or, _) -> OR;
          (_, t) -> error $ show t <> " is not comparable"

packClosure = undefined
