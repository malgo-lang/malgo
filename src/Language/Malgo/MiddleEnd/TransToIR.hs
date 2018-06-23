{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Malgo.MiddleEnd.TransToIR (trans) where

import           Language.Malgo.ID
import           Language.Malgo.IR.IR
import qualified Language.Malgo.IR.Syntax as S
import           Language.Malgo.Monad
import           Language.Malgo.Prelude
import           Language.Malgo.Type
import           Language.Malgo.TypedID

newtype TEnv = TEnv { _uniqSupply :: UniqSupply }

makeLenses ''TEnv

instance MalgoEnv TEnv where
  uniqSupplyL = uniqSupply
  genEnv = TEnv

throw :: MonadMalgo TEnv m => Doc ann -> m a
throw mes = malgoError $ "error(transToIR):" <+> mes

update :: MonadMalgo TEnv m => ID Type -> m (ID MType)
update a = do
  mty <- toMType (typeOf a)
  return $ a & idMeta .~ mty

newTmp :: MonadMalgo TEnv m => Name -> MType -> m (ID MType)
newTmp n t = ID ("$" <> n) <$> newUniq <*> return t

trans :: MonadMalgo TEnv m => S.Expr TypedID -> m (Expr (ID MType))
trans e = transToIR e

insertLet :: MonadMalgo TEnv m => S.Expr (ID Type) -> (ID MType -> m (Expr (ID MType))) -> m (Expr (ID MType))
insertLet (S.Var _ x) k = update x >>= k
insertLet v k = do
  ty <- toMType $ typeOf v
  x <- newTmp "k" ty
  v' <- transToIR v
  e <- k x
  return (Let x v' e)

transToIR :: MonadMalgo TEnv m => S.Expr TypedID -> m (Expr (ID MType))
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
  fnty <- toMType (FunTy (map snd params) (typeOf body))
  fnid <- newTmp "lambda" fnty
  return (Let fnid body' (Var fnid))
transToIR (S.Call _ fn args) =
  insertLet fn (\fn' -> bind args [] (return . Apply fn'))
  where bind [] args' k     = k (reverse args')
        bind (x:xs) args' k = insertLet x (\x' -> bind xs (x' : args') k)
transToIR (S.Seq _ e1 e2) = do
  e1' <- transToIR e1
  tmp <- newTmp "_" (mTypeOf e1')
  e2' <- transToIR e2
  return (Let tmp e1' e2')
transToIR (S.Let info (S.ValDec _ n _ val:ds) body) = do
  val' <- transToIR val
  rest <- transToIR (S.Let info ds body)
  n' <- update n
  return $ Let n' val' rest
transToIR (S.Let info (S.FunDec _ fn params _ fbody:ds) body) = do
  fbody' <- transToIR fbody
  rest <- transToIR (S.Let info ds body)
  fn' <- update fn
  params' <- mapM (update . fst) params
  return $ LetRec [(fn', Just params', fbody')] rest
transToIR (S.Let info (S.ExDec _ n _ orig:ds) body) = do
  n' <- update n
  Let n' (Prim orig (mTypeOf n')) <$> transToIR (S.Let info ds body)
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
transOp S.Add _  = Prim "add_int" (FunctionTy (IntTy 32) [IntTy 32, IntTy 32])
transOp S.Sub _  = Prim "sub_int" (FunctionTy (IntTy 32) [IntTy 32, IntTy 32])
transOp S.Mul _  = Prim "mul_int" (FunctionTy (IntTy 32) [IntTy 32, IntTy 32])
transOp S.Div _  = Prim "div_int" (FunctionTy (IntTy 32) [IntTy 32, IntTy 32])
transOp S.Mod _  = Prim "mod_int" (FunctionTy (IntTy 32) [IntTy 32, IntTy 32])
transOp S.FAdd _ = Prim "add_float" (FunctionTy DoubleTy [DoubleTy, DoubleTy])
transOp S.FSub _ = Prim "sub_float" (FunctionTy DoubleTy [DoubleTy, DoubleTy])
transOp S.FMul _ = Prim "mul_float" (FunctionTy DoubleTy [DoubleTy, DoubleTy])
transOp S.FDiv _ = Prim "div_float" (FunctionTy DoubleTy [DoubleTy, DoubleTy])
transOp S.Eq ty  = Prim ("eq_" <> show (pretty ty)) (FunctionTy (IntTy 1) [ty, ty])
transOp S.Neq ty = Prim ("neq_" <> show (pretty ty)) (FunctionTy (IntTy 1) [ty, ty])
transOp S.Lt ty  = Prim ("lt_" <> show (pretty ty)) (FunctionTy (IntTy 1) [ty, ty])
transOp S.Gt ty  = Prim ("gt_" <> show (pretty ty)) (FunctionTy (IntTy 1) [ty, ty])
transOp S.Le ty  = Prim ("le_" <> show (pretty ty)) (FunctionTy (IntTy 1) [ty, ty])
transOp S.Ge ty  = Prim ("neq_" <> show (pretty ty)) (FunctionTy (IntTy 1) [ty, ty])
transOp S.And _  = Prim "and" (FunctionTy (IntTy 1) [IntTy 1, IntTy 1])
transOp S.Or _   = Prim "or" (FunctionTy (IntTy 1) [IntTy 1, IntTy 1])

toMType :: MonadMalgo TEnv m => Type -> m MType
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
