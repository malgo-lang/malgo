{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.Old.FrontEnd.TypeCheck (typeCheck) where

import qualified Data.Map.Strict              as Map
import           Language.Malgo.Old.FrontEnd.Info
import           Language.Malgo.Old.ID
import           Language.Malgo.Old.IR.Syntax     hiding (info)
import qualified Language.Malgo.Old.IR.Syntax     as Syntax
import           Language.Malgo.Old.Monad
import           Language.Malgo.Pretty
import           Language.Malgo.Old.Type
import           Universum                    hiding (Type)

typeCheck :: Expr RawID -> MalgoM (Expr TypedID)
typeCheck e =
  runReaderT (checkExpr e) mempty

type TypeCheckM a = ReaderT (Map RawID TypedID) MalgoM a

throw :: Info -> Doc -> TypeCheckM a
throw info mes = malgoError $ "error(typecheck):" <+> pPrint info <+> mes

addBind :: RawID -> Type -> TypeCheckM a -> TypeCheckM a
addBind name typ =
  local (Map.insert name (set idMeta typ name))

addBinds :: [(RawID, Type)] -> TypeCheckM a -> TypeCheckM a
addBinds kvs =
  local (Map.fromList (map (\(name, typ) -> (name, set idMeta typ name)) kvs) <>)

getBind :: Info -> RawID -> TypeCheckM TypedID
getBind info name = do
  k <- ask
  case Map.lookup name k of
    Just x  -> return x
    Nothing -> throw info (pPrint name <+> "is not defined")

prototypes :: [Decl RawID] -> [(RawID, Type)]
prototypes xs = map mkPrototype (filter hasPrototype xs)
  where hasPrototype ExDec{}  = True
        hasPrototype FunDec{} = True
        hasPrototype _        = False
        mkPrototype (ExDec _ name ty _) = (name, ty)
        mkPrototype (FunDec _ name params retty _) = (name, FunTy (map snd params) retty)
        mkPrototype _ = error "ValDec has not prototype"

match :: (HasType a1, HasType a2) => Info -> a1 -> a2 -> TypeCheckM ()
match info a b = unless (typeOf a == typeOf b) $
  mismatchError info (typeOf a) (typeOf b)

mismatchError :: (Pretty a, Pretty b) => Info -> a -> b -> TypeCheckM c
mismatchError info expected actual = throw info $
  "expected:" <+> pPrint expected
  $+$ "actual:" <+> pPrint actual

checkDecls :: [Decl RawID] -> TypeCheckM [Decl TypedID]
checkDecls [] = return []
checkDecls (ExDec info name typ orig : ds) =
  (ExDec info (set idMeta typ name) typ orig : ) <$> checkDecls ds
checkDecls (ValDec info name Nothing val : ds) = do
  val' <- checkExpr val
  addBind name (typeOf val') $
    (ValDec info (set idMeta (typeOf val') name) Nothing val' : ) <$> checkDecls ds
checkDecls (ValDec info name (Just typ) val : ds) = do
  val' <- checkExpr val
  match info typ val'
  addBind name typ $
    (ValDec info (set idMeta typ name) (Just typ) val' : ) <$> checkDecls ds
checkDecls (FunDec info fn params retty body : ds) = do
  fnty <- makeFnTy params retty
  fd <- addBinds params $ do
    let fn' = set idMeta fnty fn
    let params' = map (\(x, t) -> (set idMeta t x, t)) params
    body' <- checkExpr body
    match info retty body'
    return $ FunDec info fn' params' retty body'
  (fd :) <$> checkDecls ds
  where
    makeFnTy [] _   = throw info "void parameter is invalid"
    makeFnTy xs ret = pure $ FunTy (map snd xs) ret

checkExpr :: Expr RawID -> TypeCheckM (Expr TypedID)
checkExpr (Var info name) = Var info <$> getBind info name
checkExpr (Int info x) = pure $ Int info x
checkExpr (Float info x) = pure $ Float info x
checkExpr (Bool info x) = pure $ Bool info x
checkExpr (Char info x) = pure $ Char info x
checkExpr (String info x) = pure $ String info x
checkExpr (Unit info) = pure $ Unit info
checkExpr (Tuple info xs) = Tuple info <$> mapM checkExpr xs
checkExpr (MakeArray info ty size) = do
  size' <- checkExpr size
  match info size' ("Int" :: Type)
  return $ MakeArray info ty size'
checkExpr (ArrayRead info arr ix) = do
  arr' <- checkExpr arr
  ix' <- checkExpr ix
  case typeOf arr' of
    ArrayTy _ -> do
      match info ix' ("Int" :: Type)
      return $ ArrayRead info arr' ix'
    t -> throw (Syntax.info arr)
         $ "expected: array"
         $+$ "actual:" <+> pPrint t
checkExpr (ArrayWrite info arr ix val) = do
  arr' <- checkExpr arr
  ix' <- checkExpr ix
  val' <- checkExpr val
  case typeOf arr' of
    ArrayTy t -> do
      match info ix' ("Int" :: Type)
      match info val' t
      return $ ArrayWrite info arr' ix' val'
    t -> throw (Syntax.info arr)
         $ "expected: array"
         $+$ "actual:" <+> pPrint t
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
      _           -> throw info $ pPrint fn' <+> "is not callable"
  mapM_ (\(arg, ty) -> match info ty arg) (zip args' paramty)
  pure (Call info fn' args')
checkExpr (TupleAccess i tuple index) = do
  tuple' <- checkExpr tuple
  case typeOf tuple' of
    TupleTy xs ->
      when (index >= length xs) $
        throw i $ "out of bounds:" <+> pPrint index <+> pPrint (TupleTy xs)
    t -> throw (Syntax.info tuple) $ "expected: tuple"
         $+$ "actual:" <+> pPrint t
  pure $ TupleAccess i tuple' index
checkExpr (BinOp info op x y) = do
  x' <- checkExpr x
  y' <- checkExpr y
  let (px, py, _) = typeOfOp info op (typeOf x')
  match info px x'
  match info py y'
  pure (BinOp info op x' y')
checkExpr (Seq info e1 e2) = do
  e1' <- checkExpr e1
  match info ("Unit" :: Type) e1'
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
  match info ("Bool" :: Type) c'
  match info t' f'
  return $ If info c' t' f'
