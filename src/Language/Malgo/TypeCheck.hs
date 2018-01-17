{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.TypeCheck (typeCheck, TypedID(..), typeOf) where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map.Strict       as Map
import           Language.Malgo.Rename
import           Language.Malgo.Syntax hiding (info)
import           Language.Malgo.Type
import           Language.Malgo.Utils
import           Text.PrettyPrint

data TypedID = TypedID ID Type
  deriving (Show, Ord)

instance Eq TypedID where
  (TypedID x _) == (TypedID y _) = x == y

instance PrettyPrint TypedID where
  pretty (TypedID x t) = pretty x <> text ":" <> pretty t

instance Typeable TypedID where
  typeOf (TypedID _ t) = t

newtype TcEnv = TcEnv { _table :: Map.Map ID TypedID }

initTcEnv :: TcEnv
initTcEnv = TcEnv
  { _table = Map.empty }

type TypeCheck a = Malgo TcEnv a

runTypeCheck :: TypeCheck a -> (Either MalgoError a, TcEnv)
runTypeCheck m = runMalgo m initTcEnv

typeCheck :: Expr ID -> (Either MalgoError (Expr TypedID), TcEnv)
typeCheck x = runTypeCheck (checkExpr x)

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
  if typ == typeOf val'
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

  if typeOf body' == retty
    then return $ FunDec info fn' params' retty body'
    else throw info $ "expected:" <+> pretty retty
                      $+$ "actual:" <+> pretty (typeOf body')

  where makeFnTy [] _         = throw info (text "void parameter is invalid")
        -- makeFnTy [(_, t)] ret = return $ FunTy t ret
        makeFnTy xs ret       =
          return $ FunTy (map snd xs) ret

instance Typeable (Expr TypedID) where
  typeOf (Var _ name) = typeOf name
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
  typeOf (BinOp i op x _) =
    case runTypeCheck (typeOfOp i op (typeOf x)) of
      (Right (FunTy _ ty), _) -> ty
      (Left mes, _)           -> error (show mes)
      _ -> error "(typeOfOp op) should match (FunTy _ ty)"

typeOfOp :: Info -> Op -> Type -> TypeCheck Type
typeOfOp _ Add _  = return $ FunTy ["Int", "Int"] "Int"
typeOfOp _ Sub _  = return $ FunTy ["Int", "Int"] "Int"
typeOfOp _ Mul _  = return $ FunTy ["Int", "Int"] "Int"
typeOfOp _ Div _  = return $ FunTy ["Int", "Int"] "Int"
typeOfOp _ FAdd _ = return $ FunTy ["Float", "Float"] "Float"
typeOfOp _ FSub _ = return $ FunTy ["Float", "Float"] "Float"
typeOfOp _ FMul _ = return $ FunTy ["Float", "Float"] "Float"
typeOfOp _ FDiv _ = return $ FunTy ["Float", "Float"] "Float"
typeOfOp _ Mod  _ = return $ FunTy ["Int", "Int"] "Int"
typeOfOp i Eq ty  = if comparable ty
                    then return $ FunTy [ty, ty] "Bool"
                    else throw i (pretty ty <+> "is not comparable")
typeOfOp i Neq ty  = if comparable ty
                     then return $ FunTy [ty, ty] "Bool"
                     else throw i (pretty ty <+> "is not comparable")
typeOfOp i Lt ty  = if comparable ty
                    then return $ FunTy [ty, ty] "Bool"
                    else throw i (pretty ty <+> "is not comparable")
typeOfOp i Gt ty  = if comparable ty
                    then return $ FunTy [ty, ty] "Bool"
                    else throw i (pretty ty <+> "is not comparable")
typeOfOp i Le ty  = if comparable ty
                    then return $ FunTy [ty, ty] "Bool"
                    else throw i (pretty ty <+> "is not comparable")
typeOfOp i Ge ty  = if comparable ty
                    then return $ FunTy [ty, ty] "Bool"
                    else throw i (pretty ty <+> "is not comparable")
typeOfOp _ And _  = return $ FunTy ["Bool", "Bool"] "Bool"
typeOfOp _ Or _   = return $ FunTy ["Bool", "Bool"] "Bool"

comparable :: Type -> Bool
comparable "Int"    = True
comparable "Float"  = True
comparable "Bool"   = True
comparable "Char"   = True
comparable "String" = True
comparable "Unit"   = False
comparable NameTy{} = False
comparable FunTy{}  = False
comparable ClsTy{}  = False

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
  -- fn' <- getBind info fn
  args' <- mapM checkExpr args
  paramty <- case typeOf fn' of
                (FunTy p _) -> return p
                _           ->
                  throw info $
                  pretty fn' <+> "is not callable"
  if map typeOf args' == paramty -- 引数が複数あるとき
     -- -- | (TupleTy (map typeOf args') == TupleTy [paramty]) -- 引数が1つのとき
    then return $ Call info fn' args'
    else throw info (text "expected:" <+> cat (punctuate (text ",") (map pretty paramty))
                     $+$ text "actual:"
                     <+> parens (cat
                                 $ punctuate (text ",")
                                 (map (pretty . typeOf) args')))
checkExpr (BinOp info op x y) = do
  x' <- checkExpr x
  y' <- checkExpr y
  (FunTy [px, py] _) <- typeOfOp info op (typeOf x')
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
