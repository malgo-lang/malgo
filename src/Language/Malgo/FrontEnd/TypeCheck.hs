{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.FrontEnd.TypeCheck (typeCheck) where

import           Control.Monad.Except
import           Data.Text.Prettyprint.Doc
import           Language.Malgo.ID
import           Language.Malgo.IR.Syntax  hiding (info)
import qualified Language.Malgo.IR.Syntax  as Syntax
import           Language.Malgo.Monad
import           Language.Malgo.Prelude    (Info)
import           Language.Malgo.Type
import           RIO
import qualified RIO.Map                   as Map
import           System.Exit

type TcEnv = Map RawID TypedID

typeCheck :: Expr RawID -> RIO MalgoApp (Expr TypedID)
typeCheck e =
  runReaderT (checkExpr e) Map.empty

type TypeCheckM ann a = ReaderT TcEnv (RIO MalgoApp) a

throw :: Info -> Doc ann -> TypeCheckM ann a
throw info mes = do
  lift $ logError $ displayShow $ "error(typecheck):" <+> pretty info <+> align mes
  liftIO exitFailure

addBind :: RawID -> Type -> TypeCheckM ann a -> TypeCheckM ann a
addBind name typ m =
  local (Map.insert name (set idMeta typ name)) m

addBinds :: [(RawID, Type)] -> TypeCheckM ann a -> TypeCheckM ann a
addBinds kvs m =
  local (Map.fromList (map (\(name, typ) -> (name, set idMeta typ name)) kvs) <>) m

getBind :: Info -> RawID -> TypeCheckM ann TypedID
getBind info name = do
  k <- ask
  case Map.lookup name k of
    Just x  -> return x
    Nothing -> throw info (pretty name <+> "is not defined")

prototypes :: [Decl RawID] -> [(RawID, Type)]
prototypes xs = map mkPrototype (filter hasPrototype xs)
  where hasPrototype ExDec{}  = True
        hasPrototype FunDec{} = True
        hasPrototype _        = False
        mkPrototype (ExDec _ name ty _) = (name, ty)
        mkPrototype (FunDec _ name params retty _) = (name, FunTy (map snd params) retty)
        mkPrototype _ = error "ValDec has not prototype"

checkDecls :: [Decl RawID] -> TypeCheckM ann [Decl TypedID]
checkDecls [] = return []
checkDecls (ExDec info name typ orig : ds) =
  (ExDec info (set idMeta typ name) typ orig : ) <$> checkDecls ds
checkDecls (ValDec info name Nothing val : ds) = do
  val' <- checkExpr val
  addBind name (typeOf val') $
    (ValDec info (set idMeta (typeOf val') name) Nothing val' : ) <$> checkDecls ds
checkDecls (ValDec info name (Just typ) val : ds) = do
  val' <- checkExpr val
  if typ == typeOf val'
    then addBind name typ $
         (ValDec info (set idMeta typ name) (Just typ) val' : ) <$> checkDecls ds
    else throw info $
         "expected:" <+> pretty typ <> line <> "actual:" <+> pretty (typeOf val')
checkDecls (FunDec info fn params retty body : ds) = do
  fnty <- makeFnTy params retty
  fd <- addBinds params $ do
    let fn' = set idMeta fnty fn
    let params' = map (\(x, t) -> (set idMeta t x, t)) params
    body' <- checkExpr body
    if typeOf body' == retty
      then pure $ FunDec info fn' params' retty body'
      else throw info $
           "expected:" <+> pretty retty
           <> line <> "actual:" <+> pretty (typeOf body')
  (fd :) <$> checkDecls ds
  where
    makeFnTy [] _   = throw info "void parameter is invalid"
    makeFnTy xs ret = pure $ FunTy (map snd xs) ret

checkExpr :: Expr RawID -> TypeCheckM ann (Expr TypedID)
checkExpr (Var info name) = Var info <$> getBind info name
checkExpr (Int info x) = pure $ Int info x
checkExpr (Float info x) = pure $ Float info x
checkExpr (Bool info x) = pure $ Bool info x
checkExpr (Char info x) = pure $ Char info x
checkExpr (String info x) = pure $ String info x
checkExpr (Unit info) = pure $ Unit info
checkExpr (Tuple info xs) = Tuple info <$> mapM checkExpr xs
checkExpr (Fn info params body) =
  addBinds params $ do
    let params' = map (\(x, t) -> (set idMeta t x, t)) params
    body' <- checkExpr body
    pure $ Fn info params' body'
checkExpr (Call info fn args) = do
  fn' <- checkExpr fn
  args' <- mapM checkExpr args
  paramty <-
    case typeOf fn' of
      (FunTy p _) -> pure p
      _           -> throw info $ pretty fn' <+> "is not callable"
  unless (map typeOf args' == paramty)
    (throw info
      ("expected:" <+> tupled (map pretty paramty)
       <> line <> "actual:" <+> tupled (map (pretty . typeOf) args')))
  pure (Call info fn' args')
checkExpr (TupleAccess i tuple index) = do
  tuple' <- checkExpr tuple
  case typeOf tuple' of
    TupleTy xs ->
      when (index >= length xs) $
        throw i $ "out of bounds:" <+> pretty index <+> pretty (TupleTy xs)
    t -> throw (Syntax.info tuple) $ "expected: tuple"
         <> line <> "actual:" <+> pretty t
  pure $ TupleAccess i tuple' index
checkExpr (BinOp info op x y) = do
    x' <- checkExpr x
    y' <- checkExpr y
    let (FunTy [px, py] _) = typeOfOp info op (typeOf x')
    when (typeOf x' /= px)
      (throw info $
        "expected:" <+> pretty px
        <> line <> "actual:" <+> pretty (typeOf x'))
    when (typeOf y' /= py)
      (throw info $
        "expected:" <+> pretty py <> line <> "actual:" <+> pretty (typeOf y'))
    pure (BinOp info op x' y')
checkExpr (Seq info e1 e2) = do
    e1' <- checkExpr e1
    unless (typeOf e1' == "Unit")
      (throw info $
        "expected:" <+>
        "Unit" <> line <> "actual:" <+> pretty (typeOf e1'))
    Seq info e1' <$> checkExpr e2
checkExpr (Let info decls e) = do
  decls' <- addBinds (prototypes decls) $ checkDecls decls
  addDecls decls' $ do
    e' <- checkExpr e
    pure (Let info decls' e')
  where addDecls decls' = addBinds (map sig decls')
        sig (FunDec _ i _ _ _) = (set idMeta () i, i ^. idMeta)
        sig (ValDec _ i _ _)   = (set idMeta () i, i ^. idMeta)
        sig (ExDec _ i _ _)    = (set idMeta () i, i ^. idMeta)
checkExpr (If info c t f) = do
  c' <- checkExpr c
  t' <- checkExpr t
  f' <- checkExpr f
  case (typeOf c' == "Bool", typeOf t' == typeOf f') of
    (True, True) -> pure (If info c' t' f')
    (True, False) -> throw info $
                     "expected:" <+>
                     pretty (typeOf t') <> line <> "actual:" <+>
                     pretty (typeOf f')
    _ -> throw info $
         "expected:" <+>
         "Bool" <> line <> "actual:" <+> pretty (typeOf c')
