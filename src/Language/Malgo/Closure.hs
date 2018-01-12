{-# LANGUAGE FlexibleContexts #-}
module Language.Malgo.Closure (conv) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict          as Map
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
                     , _varMap :: Map.Map TypedID TypedID -- クロージャ変換前と後の型の変更を記録
                     , _fundecs  :: [FunDec TypedID]
                     , _count    :: Int
                     }
  deriving Show

initClsEnv :: Int -> ClsEnv
initClsEnv = ClsEnv Map.empty [] Map.empty []

type ClsTrans a = Malgo ClsEnv a

runClosure :: Int -> ClsTrans a -> (Either MalgoError a, ClsEnv)
runClosure i m = runMalgo m (initClsEnv i)

conv
  :: Int -> H.Expr TypedID
     -> (Either MalgoError (Program TypedID), ClsEnv)
conv i x = runClosure i $ do
  x' <- convExpr x
  fs <- gets _fundecs
  return (Program fs x')

throw :: Doc -> ClsTrans a
throw = throwError . ClosureTransError

addKnown :: TypedID -> ClsTrans ()
addKnown name =
  modify $ \e -> e { _knowns = name : _knowns e }

addFunDec :: FunDec TypedID -> ClsTrans ()
addFunDec f =
  modify $ \e -> e { _fundecs = f : _fundecs e }

convID :: TypedID -> ClsTrans TypedID
convID name = do
  clss <- gets _closures
  varMap <- gets _varMap
  case Map.lookup name clss of -- クロージャになっていれば変換
    Nothing  -> case Map.lookup name varMap of -- 型が変わっていれば変換
      Nothing -> return name
      Just name' -> return name'
    Just cls -> return cls

addClsTrans :: TypedID -> TypedID -> ClsTrans ()
addClsTrans orig cls =
  modify $ \e -> e { _closures = Map.insert orig cls (_closures e) }

addVar :: TypedID -> TypedID -> ClsTrans ()
addVar x x' =
  modify $ \e -> e { _varMap = Map.insert x x' (_varMap e) }

newClsID :: TypedID -> ClsTrans TypedID
newClsID (TypedID fn fnty) = do
  let ty = toCls fnty
  c <- gets _count
  modify $ \e -> e { _count = c + 1 }
  return (TypedID
           (Internal (Language.Malgo.Rename._name fn `mappend` fromString "$cls") c)
           ty)

toCls :: Type -> Type
toCls (FunTy params ret) = ClsTy (map toCls params) (toCls ret)
toCls x = x

convExpr :: H.Expr TypedID -> ClsTrans (Expr TypedID)
convExpr (H.Let (H.ValDec x@(TypedID name (FunTy _ _)) val) body) = do
  val' <- convExpr val
  addClsTrans x (TypedID name (typeOf val')) -- 関数値はクロージャとして扱う
  addVar x (TypedID name (typeOf val'))
  body' <- convExpr body
  return (Let (ValDec (TypedID name (typeOf val')) val') body')
convExpr (H.Let (H.ValDec x@(TypedID name _) val) body) = do
  val' <- convExpr val
  addVar x (TypedID name (typeOf val'))
  body' <- convExpr body
  return (Let (ValDec (TypedID name (typeOf val')) val') body')
convExpr (H.Let (H.ExDec name orig) body) = do
  addKnown name
  Let (ExDec name orig) <$> convExpr body
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
    then CallDir <$> convID' fn <*> mapM convID args
    else CallCls <$> convID fn <*> mapM convID args
  where convID' fn = do
          varMap <- gets _varMap
          case Map.lookup fn varMap of -- 型が変わっていれば変換
            Nothing -> return fn
            Just fn' -> return fn'

convExpr (H.If c t f) =
  If <$> convID c <*> convExpr t <*> convExpr f
convExpr (H.BinOp op x y) =
  BinOp op <$> convID x <*> convID y
