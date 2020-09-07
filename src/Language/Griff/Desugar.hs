{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Desugar where

import qualified Data.Map as Map
import qualified Data.Text as T
import Language.Griff.Extension
import Language.Griff.Syntax as G
import Language.Griff.TcEnv (TcEnv, typeEnv)
import Language.Griff.Type as GT
import qualified Language.Griff.Typing as Typing
import Language.Malgo.IR.Core as C
import Language.Malgo.Id
import Language.Malgo.Monad
import Language.Malgo.Prelude
import Language.Malgo.Pretty
import Language.Malgo.TypeRep.CType as C

data DesugarEnv = DesugarEnv
  { _varEnv :: Map (Id ()) (Id CType),
    _tcEnv :: TcEnv
  }

makeLenses ''DesugarEnv

lookupName :: MonadReader DesugarEnv m => Id () -> m (Id CType)
lookupName name = do
  env <- view varEnv
  case Map.lookup name env of
    Just name' -> pure name'
    Nothing -> bug Unreachable

lookupType :: MonadReader DesugarEnv m => Id () -> m CType
lookupType name = do
  env <- view (tcEnv . typeEnv)
  case Map.lookup name env of
    Just typ -> dcType typ
    Nothing -> bug Unreachable

dcScDef :: (MonadUniq m, MonadReader DesugarEnv m) => Decl (Griff 'TypeCheck) -> m (Id CType, ([Id CType], C.Exp (Id CType)))
dcScDef (ScDef x name params expr) = do
  params' <- traverse ?? zip params paramTypes $ \(pId, pType) ->
    join $ newId <$> dcType pType <*> pure (pId ^. idName)
  local (over varEnv (Map.fromList (zip params params') <>)) $ do
    expr' <- dcExp expr
    name' <- lookupName name
    pure (name', (params', expr'))
  where
    (paramTypes, _) = splitType (view typeOf x)

dcForign :: (MonadUniq m, MonadReader DesugarEnv m) => Decl (Griff 'TypeCheck) -> m (Id CType, ([Id CType], C.Exp (Id CType)))
dcForign (Forign x@(WithType (_, primName) _) name _) = do
  name' <- lookupName name
  params <- traverse ?? paramTypes $ \paramType -> do
    join $ newId <$> dcType paramType <*> pure "$p"
  ctype <- dcType (view typeOf x)
  pure (name', (params, C.PrimCall primName ctype (map C.Var params)))
  where
    (paramTypes, _) = splitType (view typeOf x)

dcDataDef :: (MonadUniq m, MonadReader DesugarEnv m) => Decl (Griff 'TypeCheck) -> m [(Id CType, Obj (Id CType))]
dcDataDef (DataDef _ name _ cons) = do
  traverse ?? cons $ \(conName, conParams) -> do
    paramTypes <- traverse dcXType conParams
    typ <- lookupType name
    conName' <- newId (paramTypes :-> typ) (conName ^. idName)

    ps <- traverse (\t -> newId t "$p") paramTypes
    v <- newId typ "$v"
    pure (conName', Fun ps $ Let [(v, Pack typ (C.Con (T.pack $ show $ pPrint conName) paramTypes) $ map C.Var ps)] (Atom $ C.Var v))

splitType :: GT.Type -> ([GT.Type], GT.Type)
splitType (GT.TyArr t1 t2) =
  let (ps, r) = splitType t2
   in (t1 : ps, r)
splitType t = ([], t)

dcExp :: (Monad m, MonadReader DesugarEnv m) => G.Exp (Griff 'TypeCheck) -> m (C.Exp (Id CType))
dcExp (G.Var _ name) = do
  name' <- lookupName name
  case cTypeOf name' of
    [] :-> _ -> pure $ Call (C.Var name') []
    _ -> pure $ Atom $ C.Var name'

dcType :: Monad m => GT.Type -> m CType
dcType (GT.TyApp t1 t2) = undefined

dcXType :: (Monad m, MonadReader DesugarEnv m) => G.Type (Griff 'TypeCheck) -> m CType
dcXType t = do
  env <- view tcEnv
  t' <- runReaderT (Typing.transType t) env
  dcType t'