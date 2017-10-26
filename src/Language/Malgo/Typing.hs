{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
module Language.Malgo.Typing where

import           Control.Monad.State
import qualified Language.Malgo.Syntax as S
import qualified Language.Malgo.Typed  as T
import           Language.Malgo.Types

newtype TypingState = TypingState { env :: [(Name, Type)] }
  deriving Show

newtype Typing a = Typing (StateT TypingState (Either String) a)
  deriving (Functor, Applicative, Monad, MonadState TypingState)

typing :: Typing a -> Either String a
typing (Typing m) = evalStateT m (TypingState [])

addBind :: Name -> Type -> Typing ()
addBind n t = do
  ctx <- get
  put $ ctx { env = (n, t):env ctx }

error' :: Info -> String -> Typing a
error' info mes = Typing . lift . Left $ show info ++ " " ++ mes

getType :: Name -> Info -> Typing Type
getType n info = do
  ctx <- get
  case lookup n (env ctx) of
    Just ty -> return ty
    Nothing -> error' info $ "error: " ++ show n ++ " is not defined"

typeEq :: Type -> Type -> Bool
typeEq = (==)

typeError :: String -> String -> Info -> Typing a
typeError expected actual info = error' info $ "error: Expected -> " ++ expected ++ "; Actual -> " ++ actual

typeofDecl :: S.Decl -> Typing T.Decl
typeofDecl (S.DefVar i name typ val) = do
  (val', valTy) <- typeofConst val
  if typeEq typ valTy
    then return $ T.DefVar name typ val'
    else typeError (show typ) (show valTy) i
typeofDecl (S.DefFun i name retTy params body) = do
  addBind name (FunTy retTy (map snd params))
  ctx <- get
  mapM_ (uncurry addBind) params
  (body', bodyTy) <- typeofExpr body
  if typeEq retTy bodyTy
    then do put ctx
            return $ T.DefFun name retTy params body'
    else typeError (show retTy) (show bodyTy) i
typeofDecl (S.ExVar _ name typ) = addBind name typ >> return (T.ExVar name typ)
typeofDecl (S.ExFun _ fn retTy params) = addBind fn (FunTy retTy (map snd params)) >> return (T.ExFun fn retTy params)

typeofConst :: S.Const -> Typing (T.Const, Type)
typeofConst (S.Int _ x)    = return (T.Int x, IntTy)
typeofConst (S.Float _ x)  = return (T.Float x, FloatTy)
typeofConst (S.Bool _ x)   = return (T.Bool x, BoolTy)
typeofConst (S.Char _ x)   = return (T.Char x, CharTy)
typeofConst (S.String _ x) = return (T.String x, StringTy)
typeofConst (S.Unit _)     = return (T.Unit, UnitTy)
typeofConst (S.CBinOp info op x y) = do
  (x', xTy) <- typeofConst x
  (y', yTy) <- typeofConst y
  FunTy retTy [t1, t2] <- typeofOp info op xTy
  if typeEq xTy t1 && typeEq yTy t2
    then return (T.CBinOp op x' y', retTy)
    else typeError (show [t1, t2]) (show [xTy, yTy]) info

typeofOp :: Info -> Op -> Type -> Typing Type
typeofOp info op xTy =
  if | op `elem` [Add, Sub, Mul, Div, Mod] ->
       if xTy `elem` [IntTy, FloatTy]
       then return $ FunTy xTy [xTy, xTy]
       else typeError
            (show IntTy ++ " or " ++ show FloatTy)
            (show xTy)
            info
     | op `elem` [Eq, Neq, Lt, Gt, Le, Ge] ->
       if xTy `elem` [IntTy, FloatTy, CharTy]
       then return $ FunTy BoolTy [xTy, xTy]
       else typeError
            (show [IntTy, FloatTy, CharTy])
            (show xTy)
            info
     | op `elem` [And, Or] ->
       if typeEq xTy BoolTy
       then return $ FunTy BoolTy [BoolTy, BoolTy]
       else typeError
            (show BoolTy)
            (show xTy)
            info

typeofExpr :: S.Expr -> Typing (T.Expr, Type)
typeofExpr (S.Var info name) = (\ty -> (T.Var ty name, ty)) <$> getType name info
typeofExpr (S.Const c) = (\(c', ty) -> (T.Const ty c', ty)) <$> typeofConst c
typeofExpr (S.Call info fn args) = do
  FunTy retTy paramTys <- getType fn info
  args' <- mapM typeofExpr args
  if and $ zipWith typeEq paramTys (map snd args')
    then return (T.Call retTy fn (map fst args'), retTy)
    else typeError (show paramTys) (show (map snd args')) info
typeofExpr (S.Seq info e1 e2) = do
  (e1', ty1) <- typeofExpr e1
  (e2', ty2) <- typeofExpr e2
  if typeEq ty1 UnitTy
    then return (T.Seq ty2 e1' e2', ty2)
    else typeError (show UnitTy) (show ty1) info
typeofExpr (S.Let info name typ val body) = do
  (val', valTy) <- typeofExpr val
  if typeEq valTy typ
    then do addBind name typ
            (body', bodyTy) <- typeofExpr body
            return (T.Let bodyTy name typ val' body', bodyTy)
    else typeError (show typ) (show valTy) info
typeofExpr (S.If info cond t f) = do
  (cond', condTy) <- typeofExpr cond
  if typeEq condTy BoolTy
    then do (t', tt) <- typeofExpr t
            (f', ft) <- typeofExpr f
            if typeEq tt ft
              then return (T.If tt cond' t' f', tt)
              else typeError (show tt) (show ft) info
    else typeError (show BoolTy) (show condTy) info
typeofExpr (S.BinOp info op e1 e2) = do
  (e1', t1) <- typeofExpr e1
  (e2', t2) <- typeofExpr e2
  FunTy retTy [xt, yt] <- typeofOp info op t1
  if and (zipWith typeEq [xt, yt] [t1, t2])
    then return (T.BinOp retTy op e1' e2', retTy)
    else typeError (show [xt, yt]) (show [t1, t2]) info
