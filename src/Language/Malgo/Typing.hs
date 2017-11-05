{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Language.Malgo.Typing where

import           Control.Arrow              hiding ((<+>))
import           Control.Monad.State
import           Language.Malgo.PrettyPrint
import qualified Language.Malgo.Syntax      as S
import           Language.Malgo.Types
import           Text.PrettyPrint

data Decl = DefVar Name Type Const
          | DefFun Name Type [(Name, Type)] Expr
          | ExVar Name Type
          | ExFun Name Type [(Name, Type)]
  deriving (Show, Eq)

instance PrettyPrint Decl where
  pretty (DefVar name typ val) =
    parens $ text "var" <+> pretty name <> colon <> pretty typ
    <+> pretty val
  pretty (DefFun fn retTy params body) =
    parens $ text "fun" <+> parens (sep (pretty fn <> colon <> pretty retTy : map (\(n, t) -> pretty n <> colon <> pretty t) params))
    $+$ nest 4 (pretty body)
  pretty (ExVar name typ) =
    parens $ text "extern var" <+> pretty name <> colon <> pretty typ
  pretty (ExFun fn retTy params) =
    parens $ text "extern fun" <+> parens (sep (pretty fn <> colon <> pretty retTy : map (\(n, t) -> pretty n <> colon <> pretty t) params))

data Const = Int Integer
           | Float Double
           | Bool Bool
           | Char Char
           | String String
           | Unit
           | CBinOp Op Const Const
  deriving (Show, Eq)

instance PrettyPrint Const where
  pretty (Int x)         = integer x
  pretty (Float x)       = double x
  pretty (Bool True)     = text "#t"
  pretty (Bool False)    = text "#f"
  pretty (Char x)        = quotes $ char x
  pretty (String x)      = doubleQuotes $ text x
  pretty Unit            = text "()"
  pretty (CBinOp op x y) = parens (pretty op <+> pretty x <+> pretty y)

type Expr = (Expr', Type)

instance PrettyPrint Expr where
  pretty (e, t) = pretty e <> colon <> pretty t

data Expr' = Var Name
           | Const Const
           | Call Name [Expr]
           | Seq Expr Expr
           | Let Name Type Expr Expr
           | If Expr Expr Expr
           | BinOp Op Expr Expr
  deriving (Show, Eq)

instance PrettyPrint Expr' where
  pretty (Var name)     = pretty name
  pretty (Const c)        = pretty c
  pretty (Call fn args) = parens . sep $ pretty fn : map pretty args
  pretty (Seq e1 (e2, _))    =  pretty e1 $+$ pretty e2
  pretty (Let name typ val body) =
    parens $ text "let" <+> parens (pretty name <> colon <> pretty typ <+> pretty val)
    $+$ nest 2 (pretty body)
  pretty (If c t f) =
    parens $ text "if" <+> pretty c
    $+$ nest 2 (pretty t)
    $+$ nest 2 (pretty f)
  pretty (BinOp op x y) = parens (pretty op <+> pretty x <+> pretty y)

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

typeofDecl :: S.Decl -> Typing Decl
typeofDecl (S.DefVar i name typ val) = do
  (val', valTy) <- typeofConst val
  if typeEq typ valTy
    then return $ DefVar name typ val'
    else typeError (show typ) (show valTy) i
typeofDecl (S.DefFun i name retTy params body) = do
  addBind name (FunTy retTy (map snd params))
  ctx <- get
  mapM_ (uncurry addBind) params
  body'@(_, bodyTy) <- typeofExpr body
  if typeEq retTy bodyTy
    then do put ctx
            return $ DefFun name retTy params body'
    else typeError (show retTy) (show bodyTy) i
typeofDecl (S.ExVar _ name typ) = addBind name typ >> return (ExVar name typ)
typeofDecl (S.ExFun _ fn retTy params) = addBind fn (FunTy retTy (map snd params)) >> return (ExFun fn retTy params)

typeofConst :: S.Const -> Typing (Const, Type)
typeofConst (S.Int _ x)    = return (Int x, IntTy)
typeofConst (S.Float _ x)  = return (Float x, FloatTy)
typeofConst (S.Bool _ x)   = return (Bool x, BoolTy)
typeofConst (S.Char _ x)   = return (Char x, CharTy)
typeofConst (S.String _ x) = return (String x, StringTy)
typeofConst (S.Unit _)     = return (Unit, UnitTy)
typeofConst (S.CBinOp info Sub (S.Int _ 0) x) = do
  (x', xTy) <- typeofConst x
  FunTy retTy _ <- typeofOp info Sub xTy
  zero <- case xTy of
            IntTy   -> return $ Int 0
            FloatTy -> return $ Float 0
            _       -> typeError (show [IntTy, FloatTy]) (show xTy) info
  return (CBinOp Sub x' zero, retTy)
typeofConst (S.CBinOp info op x y) = do
  (x', xTy) <- typeofConst x
  (y', yTy) <- typeofConst y
  FunTy retTy [t1, t2] <- typeofOp info op xTy
  if typeEq xTy t1 && typeEq yTy t2
    then return (CBinOp op x' y', retTy)
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

typeofExpr :: S.Expr -> Typing Expr
typeofExpr (S.Var info name) = (\ty -> (Var name, ty)) <$> getType name info
typeofExpr (S.Const c) = (first Const) <$> typeofConst c
typeofExpr (S.Call info fn args) = do
  FunTy retTy paramTys <- getType fn info
  args' <- mapM typeofExpr args
  if and $ zipWith typeEq paramTys (map snd args')
    then return (Call fn args', retTy)
    else typeError (show paramTys) (show (map snd args')) info
typeofExpr (S.Seq info e1 e2) = do
  e1'@(_, ty1) <- typeofExpr e1
  e2'@(_, ty2) <- typeofExpr e2
  if typeEq ty1 UnitTy
    then return (Seq e1' e2', ty2)
    else typeError (show UnitTy) (show ty1) info
typeofExpr (S.Let info name typ val body) = do
  val'@(_, valTy) <- typeofExpr val
  if typeEq valTy typ
    then do addBind name typ
            body'@(_, bodyTy) <- typeofExpr body
            return (Let name typ val' body', bodyTy)
    else typeError (show typ) (show valTy) info
typeofExpr (S.If info cond t f) = do
  cond'@(_, condTy) <- typeofExpr cond
  if typeEq condTy BoolTy
    then do t'@(_, tt) <- typeofExpr t
            f'@(_, ft) <- typeofExpr f
            if typeEq tt ft
              then return (If cond' t' f', tt)
              else typeError (show tt) (show ft) info
    else typeError (show BoolTy) (show condTy) info
typeofExpr (S.BinOp info Sub (S.Const (S.Int _ 0)) x) = do
  x'@(_, xTy) <- typeofExpr x
  FunTy retTy _ <- typeofOp info Sub xTy
  zero <- case xTy of
            IntTy   -> return (Const $ Int 0, IntTy)
            FloatTy -> return (Const $ Float 0, FloatTy)
            _       -> typeError (show [IntTy, FloatTy]) (show xTy) info
  return (BinOp Sub x' zero, retTy)
typeofExpr (S.BinOp info op e1 e2) = do
  e1'@(_, t1) <- typeofExpr e1
  e2'@(_, t2) <- typeofExpr e2
  FunTy retTy [xt, yt] <- typeofOp info op t1
  if and (zipWith typeEq [xt, yt] [t1, t2])
    then return (BinOp op e1' e2', retTy)
    else typeError (show [xt, yt]) (show [t1, t2]) info
