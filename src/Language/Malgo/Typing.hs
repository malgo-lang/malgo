{-# LANGUAGE MultiWayIf #-}
module Language.Malgo.Typing where

import           Control.Monad.State
import           Data.Either           ()
import           Language.Malgo.Syntax

type Env = [(Name, Type)]

initEnv :: Env
initEnv = [ (mkName "print", FunTy UnitTy [StringTy])
          , (mkName "println", FunTy UnitTy [StringTy])
          , (mkName "print_int", FunTy UnitTy [IntTy])
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
    Nothing -> lift . Left $ "error: " ++ show n ++ " is not defined.\nEnv: " ++ show ctx

typeEq :: Type -> Type -> Bool
typeEq = (==)

typeError :: String -> String -> String -> StateT Env (Either String) Type
typeError expected actual info = lift . Left $ "error: Expected -> " ++ expected ++ "; Actual -> " ++ actual ++ "\n info: " ++ info

typeofExpr :: Expr -> StateT Env (Either String) Type
typeofExpr (Var name) = getType name
typeofExpr (Int _)    = return IntTy
typeofExpr (Float _)  = return FloatTy
typeofExpr (Bool _)   = return BoolTy
typeofExpr (Char _)   = return CharTy
typeofExpr (String _) = return StringTy
typeofExpr Unit = return UnitTy
typeofExpr info@(Call name args) = do
  argsTy <- mapM typeofExpr args
  funty <- getType name
  case funty of
    FunTy retTy paramsTy -> if and $ zipWith typeEq argsTy paramsTy
                            then return retTy
                            else typeError (show paramsTy) (show argsTy) (show info )
    _ -> typeError "Function" (show funty) (show info)

typeofExpr (Seq e1 e2) = do
  ty1 <- typeofExpr e1
  if typeEq ty1 UnitTy
    then typeofExpr e2
    else typeError (show UnitTy) (show ty1) (show (Seq e1 e2))

typeofExpr (If c t f) = do
  ct <- typeofExpr c
  tt <- typeofExpr t
  ft <- typeofExpr f
  if typeEq ct BoolTy
    then if typeEq tt ft
         then return tt
         else typeError (show tt) (show ft) (show (If c t f))
    else typeError (show BoolTy) (show ct) (show (If c t f))

typeofExpr info@(BinOp op e1 e2) = do
  t1 <- typeofExpr e1
  t2 <- typeofExpr e2
  if | op `elem` [Add, Sub, Mul, Div] ->
       if | typeEq t1 IntTy ->
              if typeEq t2 IntTy
              then return IntTy
              else typeError (show IntTy) (show t2) (show info)
          | typeEq t1 FloatTy ->
              if typeEq t2 FloatTy
              then return FloatTy
              else typeError (show FloatTy) (show t2) (show info)
          | otherwise ->
              typeError (show IntTy ++ " or " ++ show FloatTy) (show t1) (show info)
     | op `elem` [Eq, Lt, Gt, Le, Ge] ->
       if | t1 `elem` comparableTypes ->
            if typeEq t1 t2
            then return BoolTy
            else typeError (show t1) (show t2) (show info)
          | otherwise -> typeError ("any one of " ++ show comparableTypes) (show t1) (show info)
     | op `elem` [And, Or] ->
       if typeEq t1 BoolTy
       then if typeEq t2 BoolTy
            then return BoolTy
            else typeError (show BoolTy) (show t2) (show info)
       else typeError (show BoolTy) (show t1) (show info)
  where comparableTypes = [IntTy, FloatTy, BoolTy, CharTy, StringTy]

typeofExpr info@(Let name ty val) = do
  vt <- typeofExpr val
  if typeEq ty vt
    then addBind name ty >> return UnitTy
    else typeError (show ty) (show vt) (show info)

evalTypeofExpr :: Expr -> Either String Type
evalTypeofExpr expr = evalStateT (typeofExpr expr) initEnv

typeofDecl :: Decl -> StateT Env (Either String) Type

typeofDecl info@(Def n ty val) = do
  tv <- typeofExpr val
  if typeEq ty tv
    then addBind n ty >> return ty
    else typeError (show ty) (show tv) (show info)

typeofDecl info@(Defun fn retTy params body) = do
  let funTy = FunTy retTy (map snd params)
  ctx <- get
  put $ params ++ ctx
  addBind fn funTy
  bodyTy <- typeofExpr body
  put ctx
  if typeEq retTy bodyTy
    then do
      addBind fn funTy
      return funTy
    else typeError (show retTy) (show bodyTy) (show info)

evalTypeofDecl :: Decl -> Either String Type
evalTypeofDecl decl = evalStateT (typeofDecl decl) initEnv
