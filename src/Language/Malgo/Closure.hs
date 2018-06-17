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

import           Control.Lens                  (at, makeLenses, non, set, use,
                                                view, (%=), (.~), (?=), (^.))

import           Language.Malgo.Closure.Knowns
import           Language.Malgo.FreeVars
import qualified Language.Malgo.IR.HIR            as H
import           Language.Malgo.ID
import           Language.Malgo.IR.MIR
import           Language.Malgo.Monad
import           Language.Malgo.Prelude
import           Language.Malgo.Type
import           Language.Malgo.TypedID

data ClsEnv = ClsEnv
  { _closures   :: Map TypedID TypedID
  , _varMap     :: Map TypedID TypedID -- クロージャ変換前と後の型の変更を記録
  , _fundecs    :: [FunDec TypedID]
  , _extern     :: [ExDec TypedID]
  , _knowns     :: [TypedID] -- immutable
  , _uniqSupply :: UniqSupply
  }

makeLenses ''ClsEnv

instance MalgoEnv ClsEnv where
  uniqSupplyL = uniqSupply
  genEnv = ClsEnv mempty mempty mempty mempty mempty

type ClsTrans a = Malgo ClsEnv a

conv :: H.Expr TypedID -> ClsTrans (Program TypedID)
conv x = do
  modify (set knowns (knownFuns x))
  x' <- convExpr x
  fs <- view fundecs <$> getEnv
  exs <- view extern <$> getEnv
  ks <- view knowns <$> getEnv
  pure (Program fs exs x' ks)

throw :: Doc ann -> ClsTrans a
throw m = malgoError $ "error(closuretrans):" <+> m

addFunDec :: FunDec TypedID -> ClsTrans ()
addFunDec f = fundecs %= (f:)

addExDec :: ExDec TypedID -> ClsTrans ()
addExDec ex = extern %= (ex:)

convID :: TypedID -> ClsTrans TypedID
convID name = do
  clss <- use closures
  vm <- use varMap
  pure $ (clss <> vm) ^. (at name . non name)

addClsTrans :: TypedID -> TypedID -> ClsTrans ()
addClsTrans orig cls = (closures . at orig) ?= cls

addVar :: TypedID -> TypedID -> ClsTrans ()
addVar x x' = (varMap . at x) ?= x'

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
convExpr (H.Call fn args) =
  ifM (elem fn <$> use knowns)
  (CallDir <$> fn' <*> mapM convID args)
  (CallCls <$> cls <*> mapM convID args)
  where
    fn' = fromMaybe fn . view (at fn) <$> use varMap -- 型が変わっていれば変換
    cls = do
      cs <- use closures
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
