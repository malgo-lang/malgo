{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
module Language.Malgo.BackEnd.GenLIR where

import qualified Data.ByteString                   as B
import           Language.Malgo.BackEnd.LIRBuilder
import           Language.Malgo.ID
import           Language.Malgo.IR.HIR             as H (Lit (..))
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
      mapM_ genFunction functions
      genMainFunction mainFuncId mainExpr

genFunction M.Func{ name, captures, mutrecs, params, body } = undefined

genMainFunction mainFuncId mainExpr = do
  body <- runGenExpr mempty "x" $ genExpr mainExpr
  pure $ L.Func { name = mainFuncId, params = [], body = body }

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
  ExprEnv { captures = Just caps } <- get
  argOprs <- (caps : ) <$> mapM findVar args
  call funOpr argOprs
genExpr (M.CallClosure f args) = do
  clsPtr <- findVar f
  clsFun <- loadC clsPtr [0, 0]
  clsCap <- loadC clsPtr [0, 1]
  argOprs <- (clsCap :) <$> mapM findVar args
  call clsFun argOprs

packClosure = undefined
