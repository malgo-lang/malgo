{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
module Language.Malgo.FrontEnd.TypeCheck where

import           Control.Monad.Except        (throwError)
import           Data.List                   (nub)
import qualified Data.Map                    as Map
import           Language.Malgo.FrontEnd.Loc
import           Language.Malgo.Id
import           Language.Malgo.IR.AST
import           Language.Malgo.Monad
import           Language.Malgo.Type
import           Universum                   hiding (Type)

-- Types

data TymapEntry = Type (Type Id)
                | TyCon (TyCon Id)

data TyEnv = TyEnv { _varmap :: Map Id (TypeScheme Id)
                   , _tymap  :: Map Id TymapEntry
                   }

data TcError = UnifyError (Type Id, Type Id)
             | DuplicatedType SrcSpan Id
             | UndefinedTyCon SrcSpan Id
             | UndefinedTyVar SrcSpan Id
  deriving (Show)

type TypeCheckM a = StateT TyEnv (ExceptT TcError MalgoM) a

-- Utilities
varmap :: Lens' TyEnv (Map Id (TypeScheme Id))
varmap f (TyEnv v t) = fmap (\v' -> TyEnv v' t) (f v)

tymap :: Lens' TyEnv (Map Id TymapEntry)
tymap f (TyEnv v t) = fmap (\t' -> TyEnv v t') (f t)

newMetaVar :: TypeCheckM (TyRef a)
newMetaVar = TyRef <$> newIORef Nothing

subst :: Type Id -> Map Id (Type Id) -> TypeCheckM (Type Id)
subst (TyVar v) env =
  case Map.lookup v env of
    Just t  -> return t
    Nothing -> return (TyVar v)
subst (TyApp (TyFun ps t) ts) env = do
  t' <- subst t (Map.fromList (zip ps ts))
  subst t' env
subst (TyApp (RecordC ls) ts) env = do
  ts' <- mapM (flip subst env) ts
  return (TyApp (RecordC ls) ts')
subst (TyApp (VariantC ls) ts) env = do
  ts' <- mapM (flip subst env) ts
  return (TyApp (VariantC ls) ts')
subst (TyApp tycon ts) env =
  TyApp tycon <$> mapM (`subst` env) ts
subst (TyMeta (TyRef r)) env = do
  r' <- readIORef r
  case r' of
    Just t  -> subst t env
    Nothing -> return $ TyMeta (TyRef r)

generalize :: Type Id -> TypeCheckM (TypeScheme Id)
generalize t = do
  rs <- nub . collectMeta <$> expand t
  vs <- forM rs $ \r -> do
    tv <- newId "a"
    writeIORef r $ Just $ TyVar tv
    return tv
  return (Forall vs t)
  where
    collectMeta (TyVar _)          = []
    collectMeta (TyApp _ ts)       = concatMap collectMeta ts
    collectMeta (TyMeta (TyRef r)) = [r]

instantiate :: TypeScheme Id -> TypeCheckM (Type Id)
instantiate (Forall vs t) = do
  ms <- mapM (const $ TyMeta <$> newMetaVar) vs
  subst t (Map.fromList (zip vs ms))

(=:=) :: TypeScheme Id -> TypeScheme Id -> TypeCheckM Bool
(Forall xs t1) =:= (Forall ys t2) = do
  t2' <- subst t2 (Map.fromList (zip ys (map TyVar xs)))
  return $ t1 == t2'

expand :: Type Id -> TypeCheckM (Type Id)
expand (TyApp (TyFun ps t) ts) =
  expand =<< subst t (Map.fromList (zip ps ts))
expand (TyMeta (TyRef r)) = do
  r' <- readIORef r
  case r' of
    Just t  -> expand t
    Nothing -> return $ TyMeta $ TyRef r
expand t = return t

unify :: Type Id -> Type Id -> TypeCheckM ()
unify (TyApp (TyFun ps t) ts) t2 = do
  t1 <- subst t (Map.fromList (zip ps ts))
  unify t1 t2
unify t1 (TyApp (TyFun ps t) ts) = do
  t2 <- subst t (Map.fromList (zip ps ts))
  unify t1 t2
unify t1@(TyApp (RecordC ls1) ts1) t2@(TyApp (RecordC ls2) ts2)
  | sort ls1 == sort ls2 = do
      let ts1' = map (view _2) $ sortOn (view _1) $ zip ls1 ts1
      let ts2' = map (view _2) $ sortOn (view _1) $ zip ls2 ts2
      mapM_ (uncurry unify) (zip ts1' ts2')
  | otherwise = throwError $ UnifyError (t1, t2)
unify t1@(TyApp (VariantC ls1) ts1) t2@(TyApp (VariantC ls2) ts2)
  | sort ls1 == sort ls2 = do
      let ts1' = map (view _2) $ sortOn (view _1) $ zip ls1 ts1
      let ts2' = map (view _2) $ sortOn (view _1) $ zip ls2 ts2
      mapM_ (uncurry unify) (zip ts1' ts2')
  | otherwise = throwError $ UnifyError (t1, t2)
unify (TyApp c1 ts1) (TyApp c2 ts2)
  | c1 == c2 = mapM_ (uncurry unify) (zip ts1 ts2)
unify (TyVar x) (TyVar y) | x == y = pass
unify (TyMeta (TyRef r1)) t2 = do
  r1' <- readIORef r1
  case r1' of
    Just t1 -> unify t1 t2
    Nothing -> unifyMeta r1 t2
unify t1 t2@TyMeta{} = unify t2 t1
unify t1 t2 = throwError $ UnifyError (t1, t2)

unifyMeta :: IORef (Maybe (Type Id)) -> Type Id -> TypeCheckM ()
unifyMeta r1 (TyApp (TyFun ps t) ts) = do
  t2 <- subst t (Map.fromList $ zip ps ts)
  unifyMeta r1 t2
unifyMeta r1 (TyMeta (TyRef r2)) = do
  r2' <- readIORef r2
  whenJust r2' (unifyMeta r1)
unifyMeta r1 t2 | occur r1 t2 = throwError $ UnifyError (TyMeta (TyRef r1), t2)
                | otherwise = writeIORef r1 (Just t2)

occur :: IORef (Maybe (Type a)) -> Type a -> Bool
occur r (TyApp _ ts) = any (occur r) ts
occur _ (TyVar _) = False
occur r1 (TyMeta (TyRef r2)) | r1 == r2 = True
                             | otherwise = False

-- Functions
typeCheck :: [Decl Id] -> MalgoM (Map Id (TypeScheme Id))
typeCheck = undefined

transTy :: SType Id -> TypeCheckM (Type Id)
transTy (STyVar ss name) = do
  tm <- use tymap
  case Map.lookup name tm of
    Just (Type t) -> return t
    _             -> throwError (UndefinedTyVar ss name)
transTy (STyApp ss (SimpleC _ name) args) = do
  tm <- use tymap
  args' <- mapM transTy args
  case Map.lookup name tm of
    Just (TyCon tycon) -> return $ TyApp tycon args'
    _ -> throwError (UndefinedTyCon ss name)
-- transTy (STyApp ss (SRecordC ss xs) [])

checkDecl (TypeDef ss name ps ty) = do
  tm <- use tymap
  whenJust (Map.lookup name tm) $ \_ -> throwError $ DuplicatedType ss name
  modify (over tymap (Map.fromList (zip ps (map (Type . TyVar) ps)) <>))
  ty' <- transTy ty
  modify (over tymap (Map.singleton name (Type ty') <>))
