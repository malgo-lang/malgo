{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings     #-}

module Language.Malgo.Closure
  ( conv
  , ClsEnv(..)
  ) where

import           Control.Lens            (non, (^.), (.=), (%=), (?=), use, at, view, makeLenses)
import           Data.List               ((\\))
import           Text.PrettyPrint hiding ((<>))

import           Language.Malgo.FreeVars
import qualified Language.Malgo.HIR      as H
import           Language.Malgo.ID
import           Language.Malgo.MIR
import           Language.Malgo.Monad
import           Language.Malgo.Prelude
import           Language.Malgo.Type
import           Language.Malgo.TypedID

data ClsEnv = ClsEnv
  { _closures   :: Map TypedID TypedID
  , _knowns     :: [TypedID]
  , _varMap     :: Map TypedID TypedID -- クロージャ変換前と後の型の変更を記録
  , _fundecs    :: [FunDec TypedID]
  , _extern     :: [ExDec TypedID]
  , _uniqSupply :: UniqSupply
  }

makeLenses ''ClsEnv

instance MalgoEnv ClsEnv where
  uniqSupplyL = uniqSupply
  genEnv = ClsEnv mempty mempty mempty mempty mempty

type ClsTrans a = Malgo ClsEnv a

conv :: H.Expr TypedID -> ClsTrans (Program TypedID)
conv x = do
  x' <- convExpr x
  fs <- view fundecs <$> getEnv
  exs <- view extern <$> getEnv
  ks <- view knowns <$> getEnv
  pure (Program fs exs x' ks)

throw :: Doc -> ClsTrans a
throw m = malgoError $ "error(closuretrans):" <+> m

addKnown :: TypedID -> ClsTrans ()
-- addKnown name = modify $ \e -> e {_knowns = name : _knowns e}
addKnown name = knowns %= (name:)

addFunDec :: FunDec TypedID -> ClsTrans ()
-- addFunDec f = modify $ \e -> e {_fundecs = f : _fundecs e}
addFunDec f = fundecs %= (f:)

addExDec :: ExDec TypedID -> ClsTrans ()
-- addExDec ex = modify $ \e -> e {_extern = ex : _extern e}
addExDec ex = extern %= (ex:)

convID :: TypedID -> ClsTrans TypedID
convID name = do
  clss <- use closures
  vm <- use varMap
  -- pure $ fromMaybe name ((clss <> vm) ^. at name)-- (clss ^. at name <|> vm ^. at name)
  pure $ (clss <> vm) ^. (at name . non name)

addClsTrans :: TypedID -> TypedID -> ClsTrans ()
-- addClsTrans orig cls = modify $ \e -> e {_closures = set (at orig) (Just cls) (_closures e)}
addClsTrans orig cls = (closures . at orig) ?= cls

addVar :: TypedID -> TypedID -> ClsTrans ()
-- addVar x x' = modify $ \e -> e {_varMap = set (at x) (Just x') (_varMap e)}
addVar x x' = (varMap . at x) ?= x'

newClsID :: TypedID -> ClsTrans TypedID
newClsID (TypedID fn fnty) = do
  let ty = toCls fnty
  c <- newUniq
  pure (TypedID (ID (_name fn `mappend` "$cls") c) ty)

toCls :: Type -> Type
toCls (FunTy params ret) = ClsTy (map toCls params) (toCls ret)
toCls x                  = x

convExpr :: H.Expr TypedID -> ClsTrans (Expr TypedID)
convExpr (H.Let (H.ValDec x@(TypedID name _) val) body) = do
  val' <- convExpr val
  case typeOf val' of
    ClsTy _ _ -> addClsTrans x (TypedID name (typeOf val')) -- 関数値はクロージャとして扱う
    _         -> pure ()
  addVar x (TypedID name (typeOf val'))
  body' <- convExpr body
  pure (Let (ValDec (TypedID name (typeOf val')) val') body')
convExpr (H.Let (H.ExDec name orig) body) = do
  addKnown name
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
  (CallCls <$> convID fn <*> mapM convID args)
  where
    fn' = fromMaybe fn . view (at fn) <$> use varMap -- 型が変わっていれば変換
convExpr (H.If c t f) = If <$> convID c <*> convExpr t <*> convExpr f
convExpr (H.BinOp op x y) = BinOp op <$> convID x <*> convID y
convExpr (H.Let (H.FunDecs fd@[_]) body) = do
  backup <- get
  [clsdec@(ClsDec clsid _ _)] <- convFunDecs fd
  body' <- convExpr body
  if clsid `elem` freevars body'
    then do
      -- modify $ \env -> env {_knowns = _knowns backup}
      knowns .= backup ^. knowns
      return $ Let clsdec body'
    else return body'
convExpr (H.Let (H.FunDecs [H.FunDec x _ _]) _) = throw $ ppr x <+> "is not a function"

convFunDecs :: [H.FunDec TypedID] -> ClsTrans [Decl TypedID]
convFunDecs [H.FunDec fn@(TypedID name (FunTy paramtys ret)) params e] = do
  let efv = freevars e \\ params
  let fn' = TypedID name (FunTy (map toCls paramtys) (toCls ret))
  addVar fn fn' -- CallDirへの変換時にfnをfn'に置き換える
  clsid <- newClsID fn'
  addClsTrans fn clsid -- CallClsへの変換時にfnをclsidに置き換える
  -- eに自由変数が含まれない時、knownsにfnを追加
  e' <-
    if null efv
      then addKnown fn >> convExpr e
      else convExpr e
  fv <- mapM convID efv
  addFunDec $ FunDec fn' params fv e'
  return [ClsDec clsid fn' fv]
