{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Language.Malgo.Typing (Expr(..), Type(..), typing) where

import           Prelude               hiding (lookup)

import qualified Data.Map.Strict       as Map

import           Control.Arrow         hiding ((<+>))
import           Control.Monad.Except
import           Control.Monad.State

import qualified Language.Malgo.Syntax as S
import           Language.Malgo.Utils

import           Text.PrettyPrint

data Expr =
  -- | 変数参照
    Var Type Name
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
  | Call Type Expr [Expr]
  -- | 連続した式(e1 ; e2)
  | Seq Expr Expr
  -- | let式
  | Let Type [Decl] Expr
  -- | if式
  | If Type Expr Expr Expr
  -- | 中置演算子
  | BinOp Type Op Expr Expr
  deriving (Eq, Show)

instance PrettyPrint Expr where
  pretty (Var ty name)     = pretty name <> colon <> pretty ty
  pretty (Int x)         = integer x
  pretty (Float x)       = double x
  pretty (Bool True)     = text "#t"
  pretty (Bool False)    = text "#f"
  pretty (Char x)        = quotes $ char x
  pretty (String x)      = doubleQuotes $ text x
  pretty Unit          = text "()"
  pretty (Call ty fn arg) = parens (sep (map pretty (fn : arg))) <> colon <> pretty ty
  pretty (Seq e1 e2)    =  parens $ text "seq" $+$ pretty e1 $+$ pretty e2
  pretty (Let ty decls body) =
    parens (text "let" <+> (parens . sep $ map pretty decls)
     $+$ nest 2 (pretty body)) <> colon <> pretty ty
  pretty (If ty c t f) =
    parens (text "if" <+> pretty c
     $+$ nest 2 (pretty t)
     $+$ nest 2 (pretty f)) <> colon <> pretty ty
  pretty (BinOp ty op x y) =
    parens (sep [pretty op, pretty x, pretty y]) <> colon <> pretty ty

-- | 中置演算子の種類を表すタグ
data Op = Add | Sub | Mul | Div
        | FAdd | FSub | FMul | FDiv
        | Mod
        | Eq | Neq
        | Lt | Gt | Le | Ge
        | And | Or
  deriving (Eq, Show)

instance PrettyPrint Op where
  pretty Add  = text "+"
  pretty Sub  = text "-"
  pretty Mul  = text "*"
  pretty Div  = text "/"
  pretty FAdd = text "+."
  pretty FSub = text "-."
  pretty FMul = text "*."
  pretty FDiv = text "/."
  pretty Mod  = text "%"
  pretty Eq   = text "=="
  pretty Neq  = text "<>"
  pretty Lt   = text "<"
  pretty Gt   = text ">"
  pretty Le   = text "<="
  pretty Ge   = text ">="
  pretty And  = text "&&"
  pretty Or   = text "||"

typeof :: Expr -> Type
typeof (Var ty _)       = ty
typeof (Int _)          = NameTy "Int"
typeof (Float _)        = NameTy "Float"
typeof (Bool _)         = NameTy "Bool"
typeof (Char _)         = NameTy "Char"
typeof (String _)       = NameTy "String"
typeof Unit             = NameTy "Unit"
typeof (Call ty _ _)    = ty
typeof (Seq _ e)        = typeof e
typeof (Let ty _ _)     = ty
typeof (If ty _ _ _)    = ty
typeof (BinOp ty _ _ _) = ty

-- | Malgoの組み込みデータ型
data Type = NameTy Name
          | FunTy [Type] Type
  deriving (Eq, Show)

instance PrettyPrint Type where
  pretty (NameTy n)          = pretty n
  pretty (FunTy params ret) =
    parens (sep (map pretty params)) <+> text "->" <+> pretty ret

data Decl = FunDec Name [(Name, Type)] Type Expr
          | ValDec Name Type Expr
  deriving (Eq, Show)

instance PrettyPrint Decl where
  pretty (FunDec name params retTy body) = parens $
    text "fun" <+> (parens . sep $ pretty name <> colon <> pretty retTy : map (\(n, t) -> pretty n <> colon <> pretty t) params)
    $+$ nest 2 (pretty body)
  pretty (ValDec name typ val) = parens $
    text "val" <+> pretty name <> colon <> pretty typ <+> pretty val

newtype TypingState = TypingState { _env :: Map.Map Name Type }
  deriving Show

type Typing a = Malgo TypingState a

initEnv :: TypingState
initEnv = TypingState $ Map.fromList
  [ ( Name "print"
    , FunTy [NameTy (Name "String")]
      (NameTy (Name "Unit")))
  , ( Name "println"
    , FunTy [NameTy (Name "String")]
      (NameTy (Name "Unit")))
  , ( Name "print_int"
    , FunTy [NameTy (Name "Int")]
      (NameTy (Name "Unit")))
  , ( Name "print_float"
    , FunTy [NameTy (Name "Float")]
      (NameTy (Name "Unit")))

  ]

runTyping :: Typing a -> (Either MalgoError a, TypingState)
runTyping m = runMalgo m initEnv

typing :: S.Expr -> (Either MalgoError Expr, TypingState)
typing e = runTyping (transExpr e)

addBind :: Name -> Type -> Typing ()
addBind n t = do
  ctx <- get
  put $ ctx { _env = Map.insert n t (_env ctx) }

lookup :: Name -> Info -> Typing Type
lookup name info = do
  env <- gets _env
  case Map.lookup name env of
    Just ty -> return ty
    Nothing -> throw info $ pretty name <> text " is not defined"

throw :: Info -> Doc -> Typing a
throw info mes = throwError $ TypingError $ pretty info <+> mes

mismatch :: Info -> Doc -> Doc -> Typing a
mismatch info expected actual =
  throw info $ text "Expected" <+> text "->" <+> expected
  $+$ text "Actual" <+> text "->" <+> actual

transExpr :: S.Expr -> Typing Expr
transExpr (S.Var info name) = do
  ty <- lookup name info
  return $ Var ty name
transExpr (S.Int _ x) = return $ Int x
transExpr (S.Float _ x) = return $ Float x
transExpr (S.Bool _ x) = return $ Bool x
transExpr (S.Char _ x) = return $ Char x
transExpr (S.String _ x) = return $ String x
transExpr (S.Unit _) = return Unit
transExpr (S.Call info fn args) = do
  fn' <- transExpr fn
  let ty = typeof fn'
  (FunTy paramtys retty) <-
    case ty of
      FunTy{} -> return ty
      _       -> mismatch info (text "_ -> _") (pretty ty)
  args' <- mapM transExpr args
  if paramtys == (case map typeof args' of
                    [] -> [NameTy (Name "Unit")]
                    xs -> xs)
    then return $ Call retty fn' args'
    else mismatch info
         (sep (map pretty paramtys))
         (sep (map (pretty . typeof) args'))
transExpr (S.Seq info e1 e2) = do
  e1' <- transExpr e1
  e2' <- transExpr e2
  if typeof e1' == NameTy (Name "Unit")
    then return (Seq e1' e2')
    else mismatch info (text "Unit") (pretty $ typeof e1')
transExpr (S.Let _ decls body) = do
  env <- get
  decls' <- mapM transDecl decls
  body' <- transExpr body
  put env
  return $ Let (typeof body') decls' body'
transExpr (S.If info c t f) = do
  c' <- transExpr c
  if typeof c' == NameTy "Bool"
    then do t' <- transExpr t
            f' <- transExpr f
            if typeof t' == typeof f'
              then return (If (typeof t') c' t' f')
              else mismatch info (pretty t') (pretty f')
    else mismatch info (text "Bool") (pretty (typeof c'))
transExpr (S.BinOp info op x y) = do
  x' <- transExpr x
  y' <- transExpr y
  (op', FunTy paramtys retty) <- transOp (typeof x')
  if paramtys == [typeof x', typeof y']
    then return (BinOp retty op' x' y')
    else mismatch info (sep (map pretty paramtys)) (sep (map pretty [typeof x', typeof y']))
  where
    transOp :: Type -> Typing (Op, Type)
    transOp x =
      case (op, x) of
        (S.Add, NameTy "Int") ->
          return (Add, FunTy [NameTy "Int", NameTy "Int"] (NameTy "Int"))
        (S.Add, NameTy "Float") ->
          return (FAdd, FunTy [NameTy "Float", NameTy "Float"] (NameTy "Float"))
        (S.Add, ty) ->
          mismatch info (text "Int or Float") (pretty ty)
        (S.Sub, NameTy "Int") ->
          return (Sub, FunTy [NameTy "Int", NameTy "Int"] (NameTy "Int"))
        (S.Sub, NameTy "Float") ->
          return (FSub, FunTy [NameTy "Float", NameTy "Float"] (NameTy "Float"))
        (S.Sub, ty) ->
          mismatch info (text "Int or Float") (pretty ty)
        (S.Mul, NameTy "Int") ->
          return (Mul, FunTy [NameTy "Int", NameTy "Int"] (NameTy "Int"))
        (S.Mul, NameTy "Float") ->
          return (FMul, FunTy [NameTy "Float", NameTy "Float"] (NameTy "Float"))
        (S.Mul, ty) ->
          mismatch info (text "Int or Float") (pretty ty)
        (S.Div, NameTy "Int") ->
          return (Div, FunTy [NameTy "Int", NameTy "Int"] (NameTy "Int"))
        (S.Div, NameTy "Float") ->
          return (FDiv, FunTy [NameTy "Float", NameTy "Float"] (NameTy "Float"))
        (S.Div, ty) ->
          mismatch info (text "Int or Float") (pretty ty)
        (S.Mod, NameTy "Float") ->
          return (Mod, FunTy [NameTy "Float", NameTy "Float"] (NameTy "Float"))
        (S.Mod, ty) ->
          mismatch info (text "Float") (pretty ty)
        (S.Eq, ty) ->
          if ty `elem` map NameTy ["Int", "Float", "Char"]
          then return (Eq, FunTy [ty, ty] (NameTy "Bool"))
          else mismatch info (text "Int or Float or Char") (pretty ty)
        (S.Neq, ty) ->
          if ty `elem` map NameTy ["Int", "Float", "Char"]
          then return (Neq, FunTy [ty, ty] (NameTy "Bool"))
          else mismatch info (text "Int or Float or Char") (pretty ty)
        (S.Lt, ty) ->
          if ty `elem` map NameTy ["Int", "Float", "Char"]
          then return (Lt, FunTy [ty, ty] (NameTy "Bool"))
          else mismatch info (text "Int or Float or Char") (pretty ty)
        (S.Gt, ty) ->
          if ty `elem` map NameTy ["Int", "Float", "Char"]
          then return (Gt, FunTy [ty, ty] (NameTy "Bool"))
          else mismatch info (text "Int or Float or Char") (pretty ty)
        (S.Le, ty) ->
          if ty `elem` map NameTy ["Int", "Float", "Char"]
          then return (Le, FunTy [ty, ty] (NameTy "Bool"))
          else mismatch info (text "Int or Float or Char") (pretty ty)
        (S.Ge, ty) ->
          if ty `elem` map NameTy ["Int", "Float", "Char"]
          then return (Ge, FunTy [ty, ty] (NameTy "Bool"))
          else mismatch info (text "Int or Float or Char") (pretty ty)
        (S.And, NameTy "Bool") ->
          return (And, FunTy [NameTy "Bool", NameTy "Bool"] (NameTy "Bool"))
        (S.And, ty) ->
          mismatch info (text "Bool") (pretty ty)
        (S.Or, NameTy "Bool") ->
          return (Or, FunTy [NameTy "Bool", NameTy "Bool"] (NameTy "Bool"))
        (S.Or, ty) ->
          mismatch info (text "Bool") (pretty ty)


transType :: S.Type -> Type
transType (S.NameTy name) = NameTy name

transDecl :: S.Decl -> Typing Decl
transDecl (S.ValDec info name ty val) = do
  val' <- transExpr val
  if transType ty == typeof val'
    then do
      addBind name (typeof val')
      return $ ValDec name (transType ty) val'
    else mismatch info (pretty ty) (pretty (typeof val'))
transDecl (S.FunDec info name params retty body) = do
  let funty = FunTy (map (transType . snd) params) (transType retty)
  addBind name funty
  mapM_ (uncurry addBind . (fst &&& (transType . snd))) params
  body' <- transExpr body
  if transType retty == typeof body'
    then return $ FunDec name (map (fst &&& (transType . snd)) params) (transType retty) body'
    else mismatch info (pretty retty) (pretty (typeof body'))
