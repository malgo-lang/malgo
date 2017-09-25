module Language.Malgo.Typing where

import           Control.Monad.State
import           Data.Either
import           Language.Malgo.Syntax

type Env = [(Name, Type)]

initEnv = [ ("print", FunTy UnitTy [StringTy])
          , ("println", FunTy UnitTy [StringTy])
          , ("print_int", FunTy UnitTy [IntTy])
          ]

addBind :: Name -> Type -> StateT Env (Either String) ()
addBind n t = do
  ctx <- get
  put $ (n, t):ctx

getType :: Name -> StateT Env (Either String) Type
getType n = do
  ctx <- get
  case lookup n ctx of
    Just ty -> return ty
    Nothing -> lift . Left $ "error: " ++ n ++ " is not defined.\nEnv: " ++ show ctx

typeEq :: Type -> Type -> Bool
typeEq = (==)

typeError expected actual info = lift . Left $ "error: Expected -> " ++ expected ++ "; Actual -> " ++ actual ++ "\n info: " ++ info

typeof :: Expr -> StateT Env (Either String) Type
typeof (Var name) = getType name
typeof (Int _)    = return IntTy
typeof (Float _)  = return FloatTy
typeof (Bool _)   = return BoolTy
typeof (Char _)   = return CharTy
typeof (String _) = return StringTy
typeof Unit = return UnitTy
typeof (Call name args) = do
  argsTy <- mapM typeof args
  funty <- getType name
  case funty of
    FunTy retTy paramsTy -> if and $ zipWith typeEq argsTy paramsTy
                            then return retTy
                            else typeError (show paramsTy) (show argsTy) (show (Call name args))
typeof (Seq e1 e2) = do
  ty1 <- typeof e1
  if typeEq ty1 UnitTy
    then typeof e2
    else typeError (show UnitTy) (show ty1) (show (Seq e1 e2))
typeof (If c t f) = do
  ct <- typeof c
  tt <- typeof t
  ft <- typeof f
  if typeEq ct BoolTy
    then if typeEq tt ft
         then return tt
         else typeError (show tt) (show ft) (show (If c t f))
    else typeError (show BoolTy) (show ct) (show (If c t f))
typeof (Add e1 e2) = do
  t1 <- typeof e1
  t2 <- typeof e2
  if typeEq t1 t2
    then if typeEq t1 IntTy || typeEq t1 FloatTy
         then return t1
         else typeError (show IntTy ++ " or " ++ show FloatTy) (show t1) (show (Add e1 e2))
    else typeError (show t1) (show t2) (show (Add e1 e2))
typeof (Sub e1 e2) = do
  t1 <- typeof e1
  t2 <- typeof e2
  if typeEq t1 t2
    then if typeEq t1 IntTy || typeEq t1 FloatTy
         then return t1
         else typeError (show IntTy ++ " or " ++ show FloatTy) (show t1) (show (Sub e1 e2))
    else typeError (show t1) (show t2) (show (Sub e1 e2))
typeof (Mul e1 e2) = do
  t1 <- typeof e1
  t2 <- typeof e2
  if typeEq t1 t2
    then if typeEq t1 IntTy || typeEq t1 FloatTy
         then return t1
         else typeError (show IntTy ++ " or " ++ show FloatTy) (show t1) (show (Mul e1 e2))
    else typeError (show t1) (show t2) (show (Mul e1 e2))
typeof (Div e1 e2) = do
  t1 <- typeof e1
  t2 <- typeof e2
  if typeEq t1 t2
    then if typeEq t1 IntTy || typeEq t1 FloatTy
         then return t1
         else typeError (show IntTy ++ " or " ++ show FloatTy) (show t1) (show (Div e1 e2))
    else typeError (show t1) (show t2) (show (Div e1 e2))

typeCheckExpr expr = evalStateT (typeof expr) initEnv
