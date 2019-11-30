{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Malgo.MiddleEnd.TransToIR ( TransToIR ) where

import           Control.Lens                 (set)
import           Language.Malgo.ID
import           Language.Malgo.IR.IR
import qualified Language.Malgo.IR.Syntax     as S
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Pretty
import           Language.Malgo.TypeRep.MType
import           Language.Malgo.TypeRep.Type
import           Relude

data TransToIR

instance Pass TransToIR (S.Expr TypedID) (Expr (ID MType)) where
  isDump = dumpKNormal
  trans = fmap flattenExpr . transToIR

throw :: MonadMalgo m => Doc -> m a
throw mes = malgoError $ "error(transToIR):" <+> mes

update :: (MonadMalgo m, HasType a) => ID a -> m (ID MType)
update a = do
  mty <- toMType (typeOf a)
  return (set idMeta mty a)

newTmp :: MonadMalgo f => Text -> a -> f (ID a)
newTmp n t = newID t ("$" <> n)

insertLet :: (MonadMalgo m, HasType a) => S.Expr (ID a) -> (ID MType -> m (Expr (ID MType))) -> m (Expr (ID MType))
insertLet (S.Var _ x) k = update x >>= k
insertLet v k = do
  v' <- transToIR v
  x <- newTmp "k" (mTypeOf v')
  e <- k x
  return (Let x v' e)

transToIR :: (MonadMalgo m, HasType a) => S.Expr (ID a) -> m (Expr (ID MType))
transToIR (S.Var _ a)   = Var <$> update a
transToIR (S.Int _ x)   = return (Int x)
transToIR (S.Float _ x) = return (Float x)
transToIR (S.Bool _ x)  = return (Bool x)
transToIR (S.Char _ c)  = return (Char c)
transToIR (S.String _ s) = return (String s)
transToIR (S.Unit _) = return Unit
transToIR (S.Tuple _ vs) = bind vs [] $ return . Tuple
  where
    bind [] args k = k (reverse args)
    bind (x:xs) args k =
      insertLet x (\x' -> bind xs (x' : args) k)
transToIR (S.TupleAccess _ e i) =
  insertLet e (\e' -> return $ Access e' [0, i])
transToIR (S.MakeArray _ ty size) = do
  ty' <- toMType ty
  insertLet size $ \size' ->
    return $ MakeArray ty' size'
transToIR (S.ArrayRead _ arr ix) =
  insertLet arr $ \arr' ->
  insertLet ix $ \ix' ->
  return $ Read arr' ix'
transToIR (S.ArrayWrite _ arr ix val) =
  insertLet arr $ \arr' ->
  insertLet ix $ \ix' ->
  insertLet val $ \val' ->
  return $ Write arr' ix' val'
transToIR (S.Fn _ params body) = do
  body' <- transToIR body
  params' <- mapM (update . fst) params
  fnid <- newTmp "lambda" (FunctionTy (mTypeOf body') (map mTypeOf params'))
  return (LetRec [(fnid, params', body')] (Var fnid))
transToIR (S.Call _ fn args) =
  insertLet fn (\fn' -> bind args [] (return . Apply fn'))
  where bind [] args' k     = k (reverse args')
        bind (x:xs) args' k = insertLet x (\x' -> bind xs (x' : args') k)
transToIR (S.Seq _ e1 e2) =
  insertLet e1 (\_ -> transToIR e2)
transToIR (S.Let info (S.ValDec _ n _ val:ds) body) = do
  val' <- transToIR val
  rest <- transToIR (S.Let info ds body)
  n' <- update n
  return $ Let n' val' rest
transToIR (S.Let info decs@(S.FunDec{}:_) body) = do
  fundecs' <- mapM transFunDec fundecs
  rest' <- transToIR (S.Let info rest body)
  return $ LetRec fundecs' rest'
  where fundecs = takeWhile (\case { S.FunDec{} -> True; _ -> False }) decs
        rest = dropWhile (\case { S.FunDec{} -> True; _ -> False }) decs
        transFunDec (S.FunDec _ fn params _ fbody) = do
          fbody' <- transToIR fbody
          fn' <- update fn
          params' <- mapM (update . fst) params
          return (fn', params', fbody')
        transFunDec _ = throw "unreachable"
transToIR (S.Let info (S.ExDec _ n _ orig:ds) body) = do
  n' <- update n
  case mTypeOf n' of
    FunctionTy _ params -> do
      params' <- mapM (newTmp "x") params
      prim <- newTmp "prim" (mTypeOf n')
      LetRec [(n', params', Let prim (Prim orig (mTypeOf n')) (Apply prim params'))]
        <$> transToIR (S.Let info ds body)
    _ -> Let n' (Prim orig (mTypeOf n')) <$> transToIR (S.Let info ds body)
transToIR (S.Let _ [] body) =
  transToIR body
transToIR (S.If _ c t f) =
  insertLet c (\c' -> If c' <$> transToIR t <*> transToIR f)
transToIR (S.BinOp _ op x y) = do
  xty <- mTypeOf <$> transToIR x
  let op' = transOp op xty
  opval <- newTmp "op" (mTypeOf op')
  insertLet x $ \x' ->
    insertLet y $ \y' ->
    return $ Let opval op' (Apply opval [x', y'])

transOp :: S.Op -> MType -> Expr (ID MType)
transOp S.Add _  = Prim "add_i64" (FunctionTy (IntTy 64) [IntTy 64, IntTy 64])
transOp S.Sub _  = Prim "sub_i64" (FunctionTy (IntTy 64) [IntTy 64, IntTy 64])
transOp S.Mul _  = Prim "mul_i64" (FunctionTy (IntTy 64) [IntTy 64, IntTy 64])
transOp S.Div _  = Prim "div_i64" (FunctionTy (IntTy 64) [IntTy 64, IntTy 64])
transOp S.Mod _  = Prim "mod_i64" (FunctionTy (IntTy 64) [IntTy 64, IntTy 64])
transOp S.FAdd _ = Prim "add_double" (FunctionTy DoubleTy [DoubleTy, DoubleTy])
transOp S.FSub _ = Prim "sub_double" (FunctionTy DoubleTy [DoubleTy, DoubleTy])
transOp S.FMul _ = Prim "mul_double" (FunctionTy DoubleTy [DoubleTy, DoubleTy])
transOp S.FDiv _ = Prim "div_double" (FunctionTy DoubleTy [DoubleTy, DoubleTy])
transOp S.Eq ty  = Prim (show $ "eq_" <> pPrint ty) (FunctionTy (IntTy 1) [ty, ty])
transOp S.Neq ty = Prim (show $ "neq_" <> pPrint ty) (FunctionTy (IntTy 1) [ty, ty])
transOp S.Lt ty  = Prim (show $ "lt_" <> pPrint ty) (FunctionTy (IntTy 1) [ty, ty])
transOp S.Gt ty  = Prim (show $ "gt_" <> pPrint ty) (FunctionTy (IntTy 1) [ty, ty])
transOp S.Le ty  = Prim (show $ "le_" <> pPrint ty) (FunctionTy (IntTy 1) [ty, ty])
transOp S.Ge ty  = Prim (show $ "ge_" <> pPrint ty) (FunctionTy (IntTy 1) [ty, ty])
transOp S.And _  = Prim "and" (FunctionTy (IntTy 1) [IntTy 1, IntTy 1])
transOp S.Or _   = Prim "or" (FunctionTy (IntTy 1) [IntTy 1, IntTy 1])

toMType :: MonadMalgo f => Language.Malgo.TypeRep.Type.Type -> f MType
toMType t@(TyApp c []) =
  case c of
    IntC    -> return $ IntTy 64
    FloatC  -> return DoubleTy
    BoolC   -> return $ IntTy 1
    CharC   -> return $ IntTy 8
    StringC -> return $ PointerTy (IntTy 8)
    TupleC  -> return $ StructTy []
    _       -> throw $ pPrint t <+> "is not valid type"
toMType (TyApp (FunC ret) params) = FunctionTy <$> toMType ret <*> mapM toMType params
toMType (TyApp TupleC xs) = PointerTy . StructTy <$> mapM toMType xs
toMType (TyApp ArrayC [t]) = PointerTy <$> toMType t
toMType t = throw $ pPrint t <+> "is not valid type"
