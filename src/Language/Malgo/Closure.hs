{-# LANGUAGE FlexibleContexts #-}
module Language.Malgo.Closure (conv) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict          as Map
import           Data.Maybe
import           Data.String
import           Language.Malgo.FreeVars
import qualified Language.Malgo.HIR       as H
import           Language.Malgo.MIR
import           Language.Malgo.Rename    (ID (..))
import           Language.Malgo.Type
import           Language.Malgo.TypeCheck (TypedID (..))
import           Language.Malgo.Utils
import           Text.PrettyPrint

data ClsEnv = ClsEnv { _closures :: Map.Map TypedID TypedID
                     , _knowns   :: [TypedID]
                     , _varMap   :: Map.Map TypedID TypedID -- クロージャ変換前と後の型の変更を記録
                     , _fundecs  :: [FunDec TypedID]
                     , _extern   :: [ExDec TypedID]
                     }
  deriving Show

instance Env ClsEnv where
  initEnv = ClsEnv Map.empty [] Map.empty [] []

type ClsTrans m a = MalgoT ClsEnv m a

conv :: Monad m => H.Expr TypedID -> ClsTrans m (Program TypedID)
conv x = do
  x' <- convExpr x
  fs <- gets _fundecs
  exs <- gets _extern
  return (Program fs exs x')

throw :: Monad m => Doc -> ClsTrans m a
throw = throwError . ClosureTransError

addKnown :: Monad m => TypedID -> ClsTrans m ()
addKnown name =
  modify $ \e -> e { _knowns = name : _knowns e }

addFunDec :: Monad m => FunDec TypedID -> ClsTrans m ()
addFunDec f =
  modify $ \e -> e { _fundecs = f : _fundecs e }

addExDec :: Monad m => ExDec TypedID -> ClsTrans m ()
addExDec ex =
  modify $ \e -> e { _extern = ex : _extern e }

convID :: Monad m => TypedID -> ClsTrans m TypedID
convID name = do
  clss <- gets _closures
  varMap <- gets _varMap
  case Map.lookup name clss of -- クロージャになっていれば変換
    Nothing  -> case Map.lookup name varMap of -- 型が変わっていれば変換
      Nothing    -> return name
      Just name' -> return name'
    Just cls -> return cls

addClsTrans :: Monad m => TypedID -> TypedID -> ClsTrans m ()
addClsTrans orig cls =
  modify $ \e -> e { _closures = Map.insert orig cls (_closures e) }

addVar :: Monad m => TypedID -> TypedID -> ClsTrans m ()
addVar x x' =
  modify $ \e -> e { _varMap = Map.insert x x' (_varMap e) }

newClsID :: Monad m => TypedID -> ClsTrans m TypedID
newClsID (TypedID fn fnty) = do
  let ty = toCls fnty
  c <- newUniq
  return (TypedID
           (Internal (Language.Malgo.Rename._name fn `mappend` fromString "$cls") c)
           ty)

toCls :: Type -> Type
toCls (FunTy params ret) = ClsTy (map toCls params) (toCls ret)
toCls x                  = x

convExpr :: Monad m => H.Expr TypedID -> ClsTrans m (Expr TypedID)
convExpr (H.Let (H.ValDec x@(TypedID name _) val) body) = do
  val' <- convExpr val
  case typeOf val' of
    ClsTy _ _ -> addClsTrans x (TypedID name (typeOf val')) -- 関数値はクロージャとして扱う
    _         -> return ()
  addVar x (TypedID name (typeOf val'))
  body' <- convExpr body
  return (Let (ValDec (TypedID name (typeOf val')) val') body')
convExpr (H.Let (H.ExDec name orig) body) = do
  addKnown name
  addExDec (ExDec name orig)
  convExpr body
convExpr (H.Let (H.FunDec fn@(TypedID name (FunTy params ret)) args e) body) = do
  backup <- get

  -- fnが自由変数を持たないと仮定してeを変換
  addKnown fn
  e' <- convExpr e
  let e'fv = freevars e' \\ args
  e'' <- if null e'fv
          then return e' -- fnが自由変数を持たない
          else do put backup -- fnが自由変数をもつ
                  convExpr e

  let e'fv' = freevars e'' \\ (fn : args)
  let fn' = TypedID name (FunTy (map toCls params) (toCls ret)) -- 引数や返り値が関数値の場合を考慮
  addFunDec $ FunDec fn' args e'fv' e''
  addVar fn fn'
  body' <- convExpr body

  if fn' `elem` freevars body' -- bodyにfnが自由変数として出現しないなら、クロージャ宣言を省略
    then do clsid <- newClsID fn'
            addClsTrans fn clsid
            Let (ClsDec clsid fn' e'fv') <$> convExpr body
    else return body'
convExpr (H.Let (H.FunDec x _ _) _) =
  throw $ pretty x <+> text "is not a function"
convExpr (H.Var x)    = Var <$> convID x
convExpr (H.Int x)    = return (Int x)
convExpr (H.Float x)  = return (Float x)
convExpr (H.Bool x)   = return (Bool x)
convExpr (H.Char x)   = return (Char x)
convExpr (H.String x) = return (String x)
convExpr H.Unit       = return Unit
convExpr (H.Call fn args) = do
  knowns <- gets _knowns
  if fn `elem `knowns
    then CallDir <$> fn' <*> mapM convID args
    else CallCls <$> convID fn <*> mapM convID args
  where fn' = fromMaybe fn . Map.lookup fn <$> gets _varMap -- 型が変わっていれば変換
convExpr (H.If c t f) =
  If <$> convID c <*> convExpr t <*> convExpr f
convExpr (H.BinOp op x y) =
  BinOp op <$> convID x <*> convID y
