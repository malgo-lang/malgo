{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Language.Malgo.Typing where

import           Control.Arrow         hiding ((<+>))
import           Control.Monad.State

import qualified Language.Malgo.Syntax as S
import           Language.Malgo.Utils
import           Text.PrettyPrint

type Expr = (Expr', Type)

instance PrettyPrint Expr where
  pretty (e, t) = pretty e <> colon <> pretty t

data Expr' =
  -- | 変数参照
    Var Name
  -- | 32bit整数
  | Int Integer
  -- | 倍精度浮動小数点数
  | Float Double
  -- | 真(#t) 偽(#f)
  | Bool Bool
  -- | シングルクォートで囲まれた一文字
  | Char Char
  -- | ダブルクォートで囲まれた文字列
  | String String
  -- | 空の値("()")
  | Unit
  -- | 関数呼び出し
  | Call Name [Expr]
  -- | 連続した式(e1 ; e2)
  | Seq Expr Expr
  -- | let式
  | Let [Decl] Expr
  -- | if式
  | If Expr Expr Expr
  -- | 中置演算子
  | BinOp Op Expr Expr
  deriving (Eq, Show)

instance PrettyPrint Expr' where
  pretty (Var name)     = pretty name
  pretty (Int x)         = integer x
  pretty (Float x)       = double x
  pretty (Bool True)     = text "#t"
  pretty (Bool False)    = text "#f"
  pretty (Char x)        = quotes $ char x
  pretty (String x)      = doubleQuotes $ text x
  pretty Unit          = text "()"
  pretty (Call fn arg) = parens $ cat (pretty fn : map pretty arg)
  pretty (Seq e1 e2)    =  pretty e1 $+$ pretty e2
  pretty (Let decls body) =
    parens $ text "let" <+> (parens . sep $ map pretty decls)
    $+$ nest 2 (pretty body)
  pretty (If c t f) =
    parens $ text "if" <+> pretty c
    $+$ nest 2 (pretty t)
    $+$ nest 2 (pretty f)
  pretty (BinOp op x y) = parens (pretty op <+> pretty x <+> pretty y)

-- | Malgoの組み込みデータ型
data Type = NameTy Name
          | TupleTy [Type]
          | FunTy Type Type
  deriving (Eq, Show)

instance PrettyPrint Type where
  pretty (NameTy n)          = pretty n
  pretty (TupleTy types)     = parens (cat $ punctuate (text ",") $ map pretty types)
  pretty (FunTy domTy codTy) = pretty domTy <+> text "->" <+> pretty codTy

data Decl = FunDec Name [(Name, Type)] Type Expr
          | ValDec Name Type Expr
  deriving (Eq, Show)

instance PrettyPrint Decl where
  pretty (FunDec name params retTy body) = parens $
    text "fun" <+> (parens . sep $ pretty name <> colon <> pretty retTy : map (\(n, t) -> pretty n <> colon <> pretty t) params)
    $+$ nest 2 (pretty body)
  pretty (ValDec name typ val) = parens $
    text "val" <+> pretty name <> colon <> pretty typ <+> pretty val

newtype TypingState = TypingState { env :: [(Name, Type)] }
  deriving Show

newtype Typing a = Typing (StateT TypingState (Either String) a)
  deriving (Functor, Applicative, Monad, MonadState TypingState)

initEnv :: [(Name, Type)]
initEnv = [ ( Name "print"
            , FunTy (TupleTy [NameTy (Name "String")])
              (NameTy (Name "Unit")))
          , ( Name "println"
            , FunTy (TupleTy [NameTy (Name "String")])
              (NameTy (Name "Unit")))
          , ( Name "print_int"
            , FunTy (TupleTy [NameTy (Name "Int")])
              (NameTy (Name "Unit")))
          ]

typing :: Typing a -> Either String a
typing (Typing m) = evalStateT m (TypingState initEnv)

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


typeofExpr :: S.Expr -> Typing Expr
typeofExpr (S.Var info name) = do
  ty <- getType name info
  return (Var name, ty)
typeofExpr (S.Int _ x) = return (Int x, NameTy (Name "Int"))
typeofExpr (S.Float _ x) = return (Float x, NameTy (Name "Float"))
typeofExpr (S.Bool _ x) = return (Bool x, NameTy (Name "Bool"))
typeofExpr (S.Char _ x) = return (Char x, NameTy (Name "Char"))
typeofExpr (S.String _ x) = return (String x, NameTy (Name "String"))
typeofExpr (S.Unit _) = return (Unit, NameTy (Name "Unit"))
typeofExpr (S.Call info fn args) = do
  FunTy paramTy retTy <- getType fn info
  args' <- mapM typeofExpr args
  if typeEq paramTy (toType (map snd args'))
    then return (Call fn args', retTy)
    else typeError (show paramTy) (show (map snd args')) info
  where toType [] = TupleTy [NameTy (Name "Unit")]
        toType xs = TupleTy xs
typeofExpr (S.Seq info e1 e2) = do
  e1'@(_, ty1) <- typeofExpr e1
  e2'@(_, ty2) <- typeofExpr e2
  if typeEq ty1 (NameTy (Name "Unit"))
    then return (Seq e1' e2', ty2)
    else typeError (show (NameTy (Name "Unit"))) (show ty1) info
typeofExpr (S.Let _ decls body) = do
  env' <- get
  decls' <- mapM transDecl decls
  body'@(_, bodyTy) <- typeofExpr body
  put env'
  return (Let decls' body', bodyTy)
typeofExpr (S.If info cond t f) = do
  cond'@(_, condTy) <- typeofExpr cond
  if typeEq condTy (NameTy (Name "Bool"))
    then do t'@(_, tt) <- typeofExpr t
            f'@(_, ft) <- typeofExpr f
            if typeEq tt ft
              then return (If cond' t' f', tt)
              else typeError (show tt) (show ft) info
    else typeError (show (NameTy (Name "Bool"))) (show condTy) info
typeofExpr (S.BinOp info op e1 e2) = do
  e1'@(_, t1) <- typeofExpr e1
  e2'@(_, t2) <- typeofExpr e2
  FunTy paramTy retTy <- typeofOp info op t1
  if typeEq paramTy (TupleTy [t1, t2])
    then return (BinOp op e1' e2', retTy)
    else typeError (show paramTy) (show (TupleTy [t1, t2])) info

typeofOp :: Info -> Op -> Type -> Typing Type
typeofOp info op xTy =
  if | op `elem` [Add, Sub, Mul, Div, Mod] ->
       if xTy `elem` [NameTy (Name "Int"), NameTy (Name "Float")]
       then return $ FunTy (TupleTy [xTy, xTy]) xTy
       else typeError
            (show (NameTy (Name "Int")) ++ " or " ++ show (NameTy (Name "Float")))
            (show xTy)
            info
     | op `elem` [Eq, Neq, Lt, Gt, Le, Ge] ->
       if xTy `elem` map (NameTy . Name) ["Int", "Float", "Char"]
       then return $ FunTy (TupleTy [xTy, xTy]) (NameTy (Name "Bool"))
       else typeError
            (show $ map (NameTy . Name) ["Int", "Float", "Char"])
            (show xTy)
            info
     | op `elem` [And, Or] ->
       if typeEq xTy (NameTy (Name "Bool"))
       then return $ FunTy (TupleTy [NameTy (Name "Bool"), NameTy (Name "Bool")]) (NameTy (Name "Bool"))
       else typeError
            (show (NameTy (Name "Bool")))
            (show xTy)
            info

transType :: S.Type -> Type
transType (S.NameTy name) = NameTy name

transDecl :: S.Decl -> Typing Decl
transDecl (S.ValDec info name typ val) = do
  (val', valTy) <- typeofExpr val
  if typeEq (transType typ) valTy
    then do
      addBind name (transType typ)
      return $ ValDec name (transType typ) (val', valTy)
    else typeError (show typ) (show valTy) info
transDecl (S.FunDec info name params retTy body) = do
  env' <- get
  let funTy = FunTy (TupleTy (map (transType . snd) params)) (transType retTy)
  addBind name funTy
  mapM_ (uncurry addBind . (fst &&& (transType . snd))) params
  body'@(_, bodyTy) <- typeofExpr body
  if typeEq (transType retTy) bodyTy
    then return $ FunDec name (map (fst &&& (transType . snd)) params) (transType retTy) body'
    else do put env'
            typeError (show (transType retTy)) (show bodyTy) info
