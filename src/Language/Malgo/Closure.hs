{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.Malgo.Closure
  ( conv
  , ClsEnv(..)
  ) where

import           Data.IORef

import           Language.Malgo.Closure.Knowns
import           Language.Malgo.FreeVars
import           Language.Malgo.ID
import qualified Language.Malgo.IR.HIR         as H
import           Language.Malgo.IR.MIR
import           Language.Malgo.Monad
import           Language.Malgo.Prelude
import           Language.Malgo.Type
import           Language.Malgo.TypedID

data ClsEnv = ClsEnv
  { _closures   :: IORef (Map TypedID TypedID)
  , _varMap     :: IORef (Map TypedID TypedID) -- クロージャ変換前と後の型の変更を記録
  , _fundecs    :: IORef [FunDec TypedID]
  , _extern     :: IORef [ExDec TypedID]
  , _knowns     :: IORef [TypedID] -- immutable
  , _uniqSupply :: UniqSupply
  }

makeLenses ''ClsEnv

instance MalgoEnv ClsEnv where
  uniqSupplyL = uniqSupply
  genEnv i =
    ClsEnv <$> newMutVar mempty
    <*> newMutVar mempty
    <*> newMutVar mempty
    <*> newMutVar mempty
    <*> newMutVar mempty
    <*> pure i

type ClsTrans a = Malgo ClsEnv a

conv :: H.Expr TypedID -> ClsTrans (Program TypedID)
conv x = do
  env <- getEnv
  writeMutVar (view knowns env) (knownFuns x)
  x' <- convExpr x
  fs <- readMutVar =<< access fundecs
  exs <- readMutVar =<< access extern
  ks <- readMutVar =<< access knowns
  pure (Program fs exs x' ks)

throw :: Doc ann -> ClsTrans a
throw m = malgoError $ "error(closuretrans):" <+> m

addFunDec :: FunDec TypedID -> ClsTrans ()
addFunDec f = do
  fs <- readMutVar =<< access fundecs
  env <- getEnv
  writeMutVar (view fundecs env) (f:fs)

addExDec :: ExDec TypedID -> ClsTrans ()
addExDec ex = do
  es <- readMutVar =<< access extern
  env <- getEnv
  writeMutVar (view extern env) (ex:es)

convID :: TypedID -> ClsTrans TypedID
convID name = do
  env <- getEnv
  clss <- readMutVar $ view closures env
  vm <- readMutVar $ view varMap env
  pure $ (clss <> vm) ^. (at name . non name)

addClsTrans :: TypedID -> TypedID -> ClsTrans ()
addClsTrans orig cls = do
  env <- getEnv
  clss <- readMutVar $ view closures env
  writeMutVar (view closures env) (clss & at orig ?~ cls)

addVar :: TypedID -> TypedID -> ClsTrans ()
addVar x x' = do
  env <- getEnv
  vm <- readMutVar $ view varMap env
  writeMutVar (view varMap env) (vm & at x ?~ x')

newClsID :: TypedID -> ClsTrans TypedID
newClsID fn = do
  let ty = toCls (fn ^. idMeta)
  c <- newUniq
  pure (ID (fn ^. idName <> "$cls") c ty)

toCls :: Type -> Type
toCls (FunTy params ret) = ClsTy (map toCls params) (toCls ret)
toCls x                  = x

convExpr :: H.Expr TypedID -> ClsTrans (Expr TypedID)
convExpr (H.Let (H.ValDec x val) body) = do
  val' <- convExpr val
  case typeOf val' of
    ClsTy _ _ -> addClsTrans x (x & idMeta .~ typeOf val') -- 関数値はクロージャとして扱う
    _         -> pure ()
  addVar x (x & idMeta .~ typeOf val')
  body' <- convExpr body
  pure (Let (ValDec (x & idMeta .~ typeOf val') val') body')
convExpr (H.Let (H.ExDec name orig) body) = do
  addExDec (ExDec name orig)
  convExpr body
convExpr (H.Var x) = Var <$> convID x
convExpr (H.Int x) = pure (Int x)
convExpr (H.Float x) = pure (Float x)
convExpr (H.Bool x) = pure (Bool x)
convExpr (H.Char x) = pure (Char x)
convExpr (H.String x) = pure (String x)
convExpr H.Unit = pure Unit
convExpr (H.Tuple xs) = Tuple <$> mapM convID xs
convExpr (H.TupleAccess e i) = TupleAccess <$> convID e <*> pure i
convExpr (H.Call fn args) = do
  ks <- readMutVar =<< access knowns
  if fn `elem` ks
    then CallDir <$> fn' <*> mapM convID args
    else CallCls <$> cls <*> mapM convID args
  where
    fn' = do
      vm <- readMutVar =<< access varMap
      return $ fromMaybe fn (view (at fn) vm)  -- 型が変わっていれば変換
    cls = do
      cs <- readMutVar =<< access closures
      case view (at fn) cs of
        Just x  -> return x
        Nothing -> throw $ pretty fn <+> "is not translated to closure"

convExpr (H.If c t f) = If <$> convID c <*> convExpr t <*> convExpr f
convExpr (H.BinOp op x y) = BinOp op <$> convID x <*> convID y
convExpr (H.Let (H.FunDecs fd@[_]) body) = do
  [clsdec] <- convFunDecs fd
  body' <- convExpr body
  return $ Let clsdec body'
convExpr (H.Let (H.FunDecs [H.FunDec x _ _]) _) = throw $ pretty x <+> "is not a function"

convFunDecs :: [H.FunDec TypedID] -> ClsTrans [Decl TypedID]
convFunDecs [H.FunDec fn@(ID _ _ (FunTy paramtys ret)) params e] = do
  let fn' = fn & idMeta .~ FunTy (map toCls paramtys) (toCls ret)
  addVar fn fn' -- CallDirへの変換時にfnをfn'に置き換える
  clsid <- newClsID fn'
  addClsTrans fn clsid -- CallClsへの変換時にfnをclsidに置き換える
  e' <- convExpr e
  let fv = freevars e'
  addFunDec $ FunDec fn' params fv e'
  return [ClsDec clsid fn' fv]
