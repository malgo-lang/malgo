{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Malgo.MiddleEnd.TransToIR (trans) where

import           Language.Malgo.ID
import           Language.Malgo.IR.IR
import qualified Language.Malgo.IR.Syntax as S
import           Language.Malgo.Monad
import           Language.Malgo.Prelude
import           Language.Malgo.Type

throw :: MonadMalgo s m => Doc ann -> m a
throw mes = malgoError $ "error(transToIR):" <+> mes

update :: MonadMalgo UniqSupply m => ID Type -> m (ID MType)
update a = do
  mty <- toMType (typeOf a)
  return $ a & idMeta .~ mty

newTmp :: MonadMalgo s f => Name -> a -> f (ID a)
newTmp n t = ID ("$" <> n) <$> newUniq <*> return t

trans :: MonadMalgo UniqSupply m => S.Expr (ID Type) -> m (Expr (ID MType))
trans e = transToIR e

insertLet :: MonadMalgo UniqSupply m => S.Expr (ID Type) -> (ID MType -> m (Expr (ID MType))) -> m (Expr (ID MType))
insertLet (S.Var _ x) k = update x >>= k
insertLet v k = do
  v' <- transToIR v
  x <- newTmp "k" (mTypeOf v')
  e <- k x
  return (Let x v' e)

transToIR :: MonadMalgo UniqSupply m => S.Expr (ID Type) -> m (Expr (ID MType))
transToIR (S.Var _ a)   = Var <$> update a
transToIR (S.Int _ x)   = return (Int x)
transToIR (S.Float _ x) = return (Float x)
transToIR (S.Bool _ x)  = return (Bool x)
transToIR (S.Char _ c)  = return (Char c)
transToIR (S.String _ s) = return (String s)
transToIR (S.Unit _) = return Unit
transToIR (S.Tuple _ vs) = bind vs [] (return . Tuple)
  where
    bind [] args k = k (reverse args)
    bind (x:xs) args k =
      insertLet x (\x' -> bind xs (x' : args) k)
transToIR (S.TupleAccess _ e i) =
  insertLet e (\e' -> return $ Access e' [0, i])
transToIR (S.Fn _ params body) = do
  body' <- transToIR body
  params' <- mapM (update . fst) params
  fnid <- newTmp "lambda" (FunctionTy (mTypeOf body') (map mTypeOf params'))
  return (LetRec [(fnid, Just params', body')] (Var fnid))
transToIR (S.Call _ fn args) =
  insertLet fn (\fn' -> bind args [] (return . Apply fn'))
  where bind [] args' k     = k (reverse args')
        bind (x:xs) args' k = insertLet x (\x' -> bind xs (x' : args') k)
transToIR (S.Seq _ e1 e2) =
  insertLet e2 (\_ -> transToIR e1)
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
          return (fn', Just params', fbody')
        transFunDec _ = throw "unreachable"
transToIR (S.Let info (S.ExDec _ n _ orig:ds) body) = do
  n' <- update n
  case mTypeOf n' of
    FunctionTy _ params -> do
      params' <- mapM (newTmp "x") params
      prim <- newTmp "prim" (mTypeOf n')
      LetRec [(n', Just params', Let prim (Prim orig (mTypeOf n')) (Apply prim params'))]
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
transOp S.Add _  = Prim "add_i32" (FunctionTy (IntTy 32) [IntTy 32, IntTy 32])
transOp S.Sub _  = Prim "sub_i32" (FunctionTy (IntTy 32) [IntTy 32, IntTy 32])
transOp S.Mul _  = Prim "mul_i32" (FunctionTy (IntTy 32) [IntTy 32, IntTy 32])
transOp S.Div _  = Prim "div_i32" (FunctionTy (IntTy 32) [IntTy 32, IntTy 32])
transOp S.Mod _  = Prim "mod_i32" (FunctionTy (IntTy 32) [IntTy 32, IntTy 32])
transOp S.FAdd _ = Prim "add_double" (FunctionTy DoubleTy [DoubleTy, DoubleTy])
transOp S.FSub _ = Prim "sub_double" (FunctionTy DoubleTy [DoubleTy, DoubleTy])
transOp S.FMul _ = Prim "mul_double" (FunctionTy DoubleTy [DoubleTy, DoubleTy])
transOp S.FDiv _ = Prim "div_double" (FunctionTy DoubleTy [DoubleTy, DoubleTy])
transOp S.Eq ty  = Prim ("eq_" <> show (pretty ty)) (FunctionTy (IntTy 1) [ty, ty])
transOp S.Neq ty = Prim ("neq_" <> show (pretty ty)) (FunctionTy (IntTy 1) [ty, ty])
transOp S.Lt ty  = Prim ("lt_" <> show (pretty ty)) (FunctionTy (IntTy 1) [ty, ty])
transOp S.Gt ty  = Prim ("gt_" <> show (pretty ty)) (FunctionTy (IntTy 1) [ty, ty])
transOp S.Le ty  = Prim ("le_" <> show (pretty ty)) (FunctionTy (IntTy 1) [ty, ty])
transOp S.Ge ty  = Prim ("neq_" <> show (pretty ty)) (FunctionTy (IntTy 1) [ty, ty])
transOp S.And _  = Prim "and" (FunctionTy (IntTy 1) [IntTy 1, IntTy 1])
transOp S.Or _   = Prim "or" (FunctionTy (IntTy 1) [IntTy 1, IntTy 1])

toMType :: MonadMalgo UniqSupply f => Type -> f MType
toMType (NameTy n) =
  case n of
    "Int"    -> return $ IntTy 32
    "Float"  -> return DoubleTy
    "Bool"   -> return $ IntTy 1
    "Char"   -> return $ IntTy 8
    "String" -> return $ PointerTy (IntTy 8)
    "Unit"   -> return $ StructTy []
    _        -> throw $ pretty n <+> "is not valid type"
toMType (FunTy params ret) =
  FunctionTy <$> toMType ret <*> mapM toMType params
toMType (TupleTy xs) =
  PointerTy . StructTy <$> mapM toMType xs
toMType ClsTy{} =
  throw "ClsTy does not have MType"
