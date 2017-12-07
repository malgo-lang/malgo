{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.TypeCheck (typeCheck, TypedID(..), typeOf) where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map.Strict       as Map
import           Language.Malgo.Rename
import           Language.Malgo.Syntax
import           Language.Malgo.Utils
import           Text.PrettyPrint

data TypedID = TypedID ID Type
  deriving (Show, Ord, Eq)

instance PrettyPrint TypedID where
  pretty (TypedID x t) = pretty x <> text ":" <> pretty t

newtype TcEnv = TcEnv { _table :: Map.Map ID TypedID }

initTcEnv :: TcEnv
initTcEnv = TcEnv
  { _table = Map.empty }

type TypeCheck a = Malgo TcEnv a

runTypeCheck :: TypeCheck a -> (Either MalgoError a, TcEnv)
runTypeCheck m = runMalgo m initTcEnv

typeCheck :: Expr ID -> (Either MalgoError (Expr TypedID), TcEnv)
typeCheck = runTypeCheck . checkExpr

throw :: Info -> Doc -> TypeCheck a
throw info mes = throwError (TypeCheckError info mes)

addBind :: ID -> Type -> TypeCheck ()
addBind name typ =
  modify $ \e -> e {
    _table = Map.insert name (TypedID name typ) (_table e)
    }

getBind :: Info -> ID -> TypeCheck TypedID
getBind info name = do
  t <- gets _table
  case Map.lookup name t of
    Just x  -> return x
    Nothing -> throw info (pretty name <+> text "is not defined")
checkDecl :: Decl ID -> TypeCheck (Decl TypedID)
checkDecl (ExDec info name typ orig) = do
  addBind name typ
  return $ ExDec info (TypedID name typ) typ orig
checkDecl (ValDec info name typ val) = do
  val' <- checkExpr val
  if typ == (typeOf val')
    then do addBind name typ
            return $ ValDec info (TypedID name typ) typ val'
    else throw info ("expected:" <+> pretty typ
                     $+$ "actual:" <+> pretty (typeOf val'))
checkDecl (FunDec info fn params retty body) = do
  fnty <- makeFnTy params retty
  addBind fn fnty
  mapM_ (uncurry addBind) params
  let fn' = TypedID fn fnty
  let params' = map (\(x, t) -> (TypedID x t, t)) params
  body' <- checkExpr body

  if (typeOf body') == retty
    then return $ FunDec info fn' params' retty body'
    else throw info $ "expected:" <+> pretty retty
                      $+$ "actual:" <+> pretty (typeOf body')

  where makeFnTy [] _         = throw info (text "void parameter is invalid")
        makeFnTy [(_, t)] ret = return $ FunTy t ret
        makeFnTy xs ret       =
          return $ FunTy (TupleTy (map snd xs)) ret

typeOf :: Expr TypedID -> Type
typeOf (Var _ (TypedID _ ty)) = ty
typeOf (Int _ _)              = "Int"
typeOf (Float _ _)            = "Float"
typeOf (Bool _ _)             = "Bool"
typeOf (Char _ _)             = "Char"
typeOf (String _ _)           = "String"
typeOf (Unit _)               = "Unit"
typeOf (Call _ fn _) =
  case typeOf fn of
    (FunTy _ ty) -> ty
    _            -> error "(typeOf fn) should match (FunTy _ ty)"
typeOf (Seq _ _ e) = typeOf e
typeOf (Let _ _ e) = typeOf e
typeOf (If _ _ e _) = typeOf e
typeOf (BinOp _ op x _) =
  case typeOfOp op (typeOf x) of
    (FunTy _ ty) -> ty
    _            -> error "(typeOfOp op) should match (FunTy _ ty)"

typeOfOp :: Op -> Type -> Type
typeOfOp Add _  = FunTy (TupleTy ["Int", "Int"]) "Int"
typeOfOp Sub _  = FunTy (TupleTy ["Int", "Int"]) "Int"
typeOfOp Mul _  = FunTy (TupleTy ["Int", "Int"]) "Int"
typeOfOp Div _  = FunTy (TupleTy ["Int", "Int"]) "Int"
typeOfOp FAdd _ = FunTy (TupleTy ["Float", "Float"]) "Float"
typeOfOp FSub _ = FunTy (TupleTy ["Float", "Float"]) "Float"
typeOfOp FMul _ = FunTy (TupleTy ["Float", "Float"]) "Float"
typeOfOp FDiv _ = FunTy (TupleTy ["Float", "Float"]) "Float"
typeOfOp Mod  _ = FunTy (TupleTy ["Int", "Int"]) "Int"
typeOfOp Eq ty  = FunTy (TupleTy [ty, ty]) "Bool"
typeOfOp Neq ty = FunTy (TupleTy [ty, ty]) "Bool"
typeOfOp Lt ty  = FunTy (TupleTy [ty, ty]) "Bool"
typeOfOp Gt ty  = FunTy (TupleTy [ty, ty]) "Bool"
typeOfOp Le ty  = FunTy (TupleTy [ty, ty]) "Bool"
typeOfOp Ge ty  = FunTy (TupleTy [ty, ty]) "Bool"
typeOfOp And _  = FunTy (TupleTy ["Bool", "Bool"]) "Bool"
typeOfOp Or _   = FunTy (TupleTy ["Bool", "Bool"]) "Bool"

comparable :: Type -> Bool
comparable "Int"       = True
comparable "Float"     = True
comparable "Bool"      = True
comparable "Char"      = True
comparable "String"    = True
comparable "Unit"      = False
comparable (NameTy _)  = False
comparable (TupleTy _) = False
comparable (FunTy _ _) = False
comparable (ClsTy _ _) = False

checkExpr :: Expr ID -> TypeCheck (Expr TypedID)
checkExpr (Var info name) = Var info <$> getBind info name
checkExpr (Int info x)    = return $ Int info x
checkExpr (Float info x)  = return $ Float info x
checkExpr (Bool info x)   = return $ Bool info x
checkExpr (Char info x)   = return $ Char info x
checkExpr (String info x) = return $ String info x
checkExpr (Unit info)     = return $ Unit info
checkExpr (Call info fn args) = do
  fn' <- checkExpr fn
  args' <- mapM checkExpr args
  paramty <- case typeOf fn' of
                (FunTy p _) -> return p
                _           ->
                  throw info $
                  pretty (typeOf fn') <+> "is not callable"
  if (TupleTy (map typeOf args') == paramty) -- 引数が複数あるとき
     || (TupleTy (map typeOf args') == TupleTy [paramty]) -- 引数が1つのとき
    then return $ Call info fn' args'
    else throw info (text "expected:" <+> pretty paramty
                     $+$ text "actual:"
                     <+> parens (cat
                                 $ punctuate (text ",")
                                 (map (pretty . typeOf) args')))
checkExpr (BinOp info op x y) = do
  x' <- checkExpr x
  y' <- checkExpr y
  let (FunTy (TupleTy [px, py]) _) = typeOfOp op (typeOf x')
  when (typeOf x' /= px) (throw info $ text "expected:" <+> pretty px
                          $+$ text "actual:" <+> pretty (typeOf x'))
  when (typeOf y' /= py) (throw info $ text "expected:" <+> pretty py
                          $+$ text "actual:" <+> pretty (typeOf y'))
  return (BinOp info op x' y')
checkExpr (Seq info e1 e2) = do
  e1' <- checkExpr e1
  if typeOf e1' == "Unit"
    then Seq info e1' <$> checkExpr e2
    else throw info $ text "expected:" <+> text "Unit"
         $+$ "actual:" <+> pretty (typeOf e1')
checkExpr (Let info decls e) = do
  backup <- get
  decls' <- mapM checkDecl decls
  e' <- checkExpr e
  put backup
  return (Let info decls' e')
checkExpr (If info c t f) = do
  c' <- checkExpr c
  t' <- checkExpr t
  f' <- checkExpr f
  if typeOf c' == "Bool"
    then if typeOf t' == typeOf f'
         then return (If info c' t' f')
         else throw info $ text "expected:" <+> pretty (typeOf t')
              $+$ text "actual:" <+> pretty (typeOf f')
    else throw info $ text "expected:" <+> text "Bool"
         $+$ text "actual:" <+> pretty (typeOf c')
-- checkExpr (Var info name) = do
--   name'@(TypedID _ ty) <- getBind info name
--   return (Var info name', ty)
-- checkExpr (Int info x) = return (Int info x, "Int")
-- checkExpr (Float info x) = return (Float info x, "Float")
-- checkExpr (Bool )
