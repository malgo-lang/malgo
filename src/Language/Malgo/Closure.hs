{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Malgo.Closure
  ( conv, ClsEnv(..) ) where

import           Data.List               ((\\))
import           Language.Malgo.FreeVars
import qualified Language.Malgo.HIR      as H
import           Language.Malgo.ID
import           Language.Malgo.MIR
import           Language.Malgo.Monad
import           Language.Malgo.Prelude
import           Language.Malgo.Type
import           Language.Malgo.TypedID
import           Text.PrettyPrint

data ClsEnv = ClsEnv { _closures   :: Map TypedID TypedID
                     , _knowns     :: [TypedID]
                     , _varMap     :: Map TypedID TypedID -- クロージャ変換前と後の型の変更を記録
                     , _fundecs    :: [FunDec TypedID]
                     , _extern     :: [ExDec TypedID]
                     , _uniqSupply :: Int
                     } deriving (Show, Generic)

instance Default ClsEnv

instance HasUniqSupply ClsEnv where
  uniqSupply = lens _uniqSupply (\s i -> s { _uniqSupply = i })

type ClsTrans a = Malgo ClsEnv a

conv :: H.Expr TypedID -> ClsTrans (Program TypedID)
conv x = do
  x' <- convExpr x
  fs <- gets _fundecs
  exs <- gets _extern
  knowns <- gets _knowns
  pure (Program fs exs x' knowns)

throw :: Doc -> ClsTrans a
throw m = malgoError $ "error(closuretrans):" <+> m

addKnown :: TypedID -> ClsTrans ()
addKnown name = modify $ \e -> e {_knowns = name : _knowns e}

addFunDec :: FunDec TypedID -> ClsTrans ()
addFunDec f = modify $ \e -> e {_fundecs = f : _fundecs e}

addExDec :: ExDec TypedID -> ClsTrans ()
addExDec ex = modify $ \e -> e {_extern = ex : _extern e}

convID :: TypedID -> ClsTrans TypedID
convID name = do
  clss <- gets _closures
  varMap <- gets _varMap
  pure $ fromMaybe name (lookup name clss <|> lookup name varMap)

addClsTrans :: TypedID -> TypedID -> ClsTrans ()
addClsTrans orig cls =
  modify $ \e -> e {_closures = insert orig cls (_closures e)}

addVar :: TypedID -> TypedID -> ClsTrans ()
addVar x x' = modify $ \e -> e {_varMap = insert x x' (_varMap e)}

newClsID :: TypedID -> ClsTrans TypedID
newClsID (TypedID fn fnty) = do
  let ty = toCls fnty
  c <- newUniq
  pure (TypedID
        (ID (_name fn `mappend` fromString "$cls") c)
        ty)

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
  ifM (elem fn <$> gets _knowns)
    (CallDir <$> fn' <*> mapM convID args)
    (CallCls <$> convID fn <*> mapM convID args)
  where
    fn' = fromMaybe fn . lookup fn <$> gets _varMap -- 型が変わっていれば変換
convExpr (H.If c t f) = If <$> convID c <*> convExpr t <*> convExpr f
convExpr (H.BinOp op x y) = BinOp op <$> convID x <*> convID y
convExpr (H.Let (H.FunDecs fd@[_]) body) = do
  backup <- get
  [clsdec@(ClsDec clsid _ _)] <- convFunDecs fd
  body' <- convExpr body
  if clsid `elem` freevars body'
  then do modify $ \env -> env { _knowns = _knowns backup }
          return $ Let clsdec body'
  else return body'
convExpr (H.Let (H.FunDecs [H.FunDec x _ _]) _) =
  throw $ pretty x <+> text "is not a function"

convFunDecs :: [H.FunDec TypedID] -> ClsTrans [Decl TypedID]
convFunDecs [H.FunDec fn@(TypedID name (FunTy paramtys ret)) params e] = do
  let efv = freevars e \\ params

  let fn' = TypedID name (FunTy (map toCls paramtys) (toCls ret))

  addVar fn fn' -- CallDirへの変換時にfnをfn'に置き換える
  clsid <- newClsID fn'
  addClsTrans fn clsid -- CallClsへの変換時にfnをclsidに置き換える

  -- eに自由変数が含まれない時、knownsにfnを追加
  e' <- if null efv
        then addKnown fn >> convExpr e
        else convExpr e

  fv <- mapM convID efv

  addFunDec $ FunDec fn' params fv e'

  return [ClsDec clsid fn' fv]
