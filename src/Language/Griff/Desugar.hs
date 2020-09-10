{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Desugar where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Language.Griff.Extension
import Language.Griff.Grouping
import Language.Griff.Syntax as G
import Language.Griff.TcEnv (TcEnv, tyConEnv, typeEnv)
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

desugar tcEnv ds = runReaderT (dcBindGroup ds) $ DesugarEnv mempty tcEnv

dcBindGroup bg = do
  mconcat <$> traverse dcDataDef (bg ^. dataDefs)

lookupName :: MonadReader DesugarEnv m => Id () -> m (Id CType)
lookupName name = do
  env <- view varEnv
  case Map.lookup name env of
    Just name' -> pure name'
    Nothing -> bug Unreachable

lookupType :: (MonadReader DesugarEnv m, MonadFail m) => Id () -> m CType
lookupType name = do
  env <- view (tcEnv . typeEnv)
  case Map.lookup name env of
    Just typ -> pure $ dcType typ
    Nothing -> bug Unreachable

dcScDef :: (MonadUniq m, MonadReader DesugarEnv m, MonadFail m) => ScDef (Griff 'TypeCheck) -> m (Id CType, ([Id CType], C.Exp (Id CType)))
dcScDef (x, name, params, expr) = do
  params' <- traverse ?? zip params paramTypes $ \(pId, pType) ->
    newId (dcType pType) (pId ^. idName)
  local (over varEnv (Map.fromList (zip params params') <>)) $ do
    expr' <- dcExp expr
    name' <- lookupName name
    pure (name', (params', expr'))
  where
    (paramTypes, _) = splitType (view typeOf x)

dcForign :: (MonadUniq m, MonadReader DesugarEnv m, MonadFail m) => Forign (Griff 'TypeCheck) -> m (Id CType, ([Id CType], C.Exp (Id CType)))
dcForign (x@(WithType (_, primName) _), name, _) = do
  name' <- lookupName name
  params <- traverse ?? paramTypes $ \paramType -> do
    newId (dcType paramType) "$p"
  pure (name', (params, C.PrimCall primName (dcType (view typeOf x)) (map C.Var params)))
  where
    (paramTypes, _) = splitType (view typeOf x)

dcDataDef :: (MonadUniq m, MonadReader DesugarEnv m, MonadFail m) => DataDef (Griff 'TypeCheck) -> m [(Id CType, Obj (Id CType))]
dcDataDef (_, name, _, cons) = do
  traverse ?? cons $ \(conName, _) -> do
    Just (GT.TyCon name') <- Map.lookup name <$> view (tcEnv . typeEnv)
    Just (_, conMap) <- Map.lookup name' <$> view (tcEnv . tyConEnv)
    let typ = dcType $ fromJust $ List.lookup conName conMap
    conName' <- newId typ (conName ^. idName)
    case typ of
      paramTypes :-> retType -> do
        ps <- traverse (\t -> newId t "$p") paramTypes
        unfoldedType <- unfoldType $ snd $ splitTyArr (fromJust $ List.lookup conName conMap)
        v <- newId unfoldedType "$v"
        pure (conName', Fun ps $ Let [(v, Pack unfoldedType (C.Con (T.pack $ show $ pPrint conName) paramTypes) $ map C.Var ps)] (Cast retType $ C.Var v))
      _ -> do
        unfoldedType <- unfoldType $ fromJust $ List.lookup conName conMap
        v <- newId unfoldedType "$v"
        pure (conName', Fun [] $ Let [(v, Pack unfoldedType (C.Con (T.pack $ show $ pPrint conName) []) [])] (Cast typ $ C.Var v))

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

dcType :: HasCallStack => GT.Type -> CType
dcType (GT.TyApp t1 t2) = do
  let (con, ts) = splitCon t1 t2
  DataT (T.pack $ show $ pPrint con) $ map dcType ts
dcType (GT.TyVar i) = VarT $ i ^. idUniq
dcType (GT.TyCon con) | kind con == Star = DataT (T.pack $ show $ pPrint con) []
dcType (GT.TyPrim GT.Int32T) = error "Int32# is not implemented"
dcType (GT.TyPrim GT.Int64T) = C.IntT
dcType (GT.TyPrim GT.FloatT) = error "Float# is not implemented"
dcType (GT.TyPrim GT.DoubleT) = C.FloatT
dcType (GT.TyPrim GT.CharT) = C.CharT
dcType (GT.TyPrim GT.StringT) = C.StringT
dcType (GT.TyArr t1 t2) =
  map dcType ps :-> dcType r
  where
    (ps, r) = splitTyArr $ GT.TyArr t1 t2
dcType (GT.TyTuple ts) =
  SumT $ Set.singleton $ C.Con ("Tuple" <> T.pack (show $ length ts)) $ map dcType ts
dcType (GT.TyLazy t) = [] :-> dcType t
dcType GT.TyMeta {} = error "TyMeta must be removed"

splitCon :: GT.Type -> GT.Type -> (Id Kind, [GT.Type])
splitCon (GT.TyCon con) t = (con, [t])
splitCon (GT.TyApp t1 t2) t3 =
  let (dataCon, ts) = splitCon t1 t2
   in (dataCon, ts <> [t3])
splitCon _ _ = bug Unreachable

splitTyArr :: GT.Type -> ([GT.Type], GT.Type)
splitTyArr (GT.TyArr t1 t2) =
  let (ps, r) = splitTyArr t2
   in (t1 : ps, r)
splitTyArr t = ([], t)

dcXType :: MonadReader DesugarEnv m => G.Type (Griff 'TypeCheck) -> m CType
dcXType t =
  view tcEnv
    >>= runReaderT (Typing.transType t)
    >>= pure . dcType

unfoldType :: (MonadReader DesugarEnv m, MonadFail m) => GT.Type -> m CType
unfoldType (GT.TyApp t1 t2) = do
  let (con, ts) = splitCon t1 t2
  Just (as, conMap) <- Map.lookup con <$> view (tcEnv . tyConEnv)
  let conMap' = over (mapped . _2) (Typing.applySubst $ Map.fromList $ zip as ts) conMap
  pure $
    SumT $
      Set.fromList $
        map ?? conMap' $ \(conName, conType) ->
          C.Con (T.pack $ show $ pPrint conName) $ map dcType $ fst $ splitTyArr conType
unfoldType (GT.TyCon con) | kind con == Star = do
  Just ([], conMap) <- Map.lookup con <$> view (tcEnv . tyConEnv)
  pure $
    SumT $
      Set.fromList $
        map ?? conMap $ \(conName, conType) ->
          C.Con (T.pack $ show $ pPrint conName) $ map dcType $ fst $ splitTyArr conType
unfoldType t = pure $ dcType t