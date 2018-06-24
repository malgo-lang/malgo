{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Language.Malgo.TypeCheck
    ( typeCheck
    ) where

import           Control.Lens           ((^.), (.~), makeLenses)

import           Language.Malgo.ID
import           Language.Malgo.Monad
import           Language.Malgo.Prelude
import           Language.Malgo.IR.Syntax  hiding (info)
import qualified Language.Malgo.IR.Syntax  as Syntax
import           Language.Malgo.Type
import           Language.Malgo.TypedID

data TcEnv = TcEnv { _table      :: Map RawID TypedID
                   , _uniqSupply :: UniqSupply
                   }

makeLenses ''TcEnv

instance MalgoEnv TcEnv where
  uniqSupplyL = uniqSupply
  genEnv = return . TcEnv mempty

typeCheck :: MonadMalgo TcEnv m => Expr RawID -> m (Expr TypedID)
typeCheck = checkExpr

throw :: MonadMalgo TcEnv m => Info -> Doc ann -> m a
throw info mes = malgoError $ "error(typecheck):" <+> pretty info <+> mes

addBind :: MonadMalgo TcEnv m => RawID -> Type -> m a -> m a
addBind name typ m = addTable [(name, name & idMeta .~ typ)] table m
addBinds :: MonadMalgo TcEnv m => [(RawID, Type)] -> m a -> m a
addBinds kvs m = addTable (map (\(name, typ) -> (name, name & idMeta .~ typ)) kvs) table m

getBind :: MonadMalgo TcEnv m => Info -> RawID -> m TypedID
getBind info name =
  lookupTable ("error(typecheck):" <+> pretty info <+> pretty name <+> "is not defined") name table

prototypes :: [Decl RawID] -> [(RawID, Type)]
prototypes xs = map mkPrototype (filter hasPrototype xs)
  where hasPrototype ExDec{}  = True
        hasPrototype FunDec{} = True
        hasPrototype _        = False
        mkPrototype (ExDec _ name ty _) = (name, ty)
        mkPrototype (FunDec _ name params retty _) = (name, FunTy (map snd params) retty)
        mkPrototype _ = error "ValDec has not prototype"

checkDecls :: MonadMalgo TcEnv m => [Decl RawID] -> m [Decl TypedID]
checkDecls [] = pure []
checkDecls (ExDec info name typ orig : ds) =
  (ExDec info (name & idMeta .~ typ) typ orig : ) <$> checkDecls ds
checkDecls (ValDec info name Nothing val : ds) = do
  val' <- checkExpr val
  addBind name (typeOf val') $
    (ValDec info (name & idMeta .~ typeOf val') Nothing val' : ) <$> checkDecls ds
checkDecls (ValDec info name (Just typ) val : ds) = do
  val' <- checkExpr val
  if typ == typeOf val'
    then addBind name typ $
         (ValDec info (name & idMeta .~ typ) (Just typ) val' : ) <$> checkDecls ds
    else throw info $
         "expected:" <+> pretty typ <> line <> "actual:" <+> pretty (typeOf val')
checkDecls (FunDec info fn params retty body : ds) = do
  fnty <- makeFnTy params retty
  fd <- addBinds params $ do
    let fn' = fn & idMeta .~ fnty
    let params' = map (\(x, t) -> (x & idMeta .~ t, t)) params
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

checkExpr :: MonadMalgo TcEnv m => Expr RawID -> m (Expr TypedID)
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
    let params' = map (\(x, t) -> (x & idMeta .~ t, t)) params
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
        sig (FunDec _ id _ _ _) = (id & idMeta .~ (), id ^. idMeta)
        sig (ValDec _ id _ _) = (id & idMeta .~ (), id ^. idMeta)
        sig (ExDec _ id _ _) = (id & idMeta .~ (), id ^. idMeta)
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
