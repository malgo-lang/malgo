{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.FrontEnd.TypeCheck where

import qualified Data.Map.Strict                 as Map
import           Language.Malgo.FrontEnd.Loc
import           Language.Malgo.FrontEnd.RnTcEnv
import           Language.Malgo.Id
import           Language.Malgo.IR.AST
import           Language.Malgo.Monad
import           Language.Malgo.Pretty
import           Language.Malgo.Type
import           Universum                       hiding (Type)

typeCheckError :: SrcSpan -> Doc -> a
typeCheckError ss doc = error $ show $ "error(type check)[" <> pPrint ss <> "]" <+> doc

-- TODO: 型検査が終わった後、型環境内のすべてのTyRefに値が代入されていることを検査する
-- f :: forall a. a -> a
-- f x = xのxの型はTyMeta (TyRef (Just (TyVar "a")))となる
typeCheck :: MonadMalgo m => [Decl Id] -> m RnTcEnv
typeCheck ds = do
  env <- makeRnTcEnv
  executingStateT env $ do
    mapM_ generateHeader ds
    mapM_ loadTypeDef typeDefs
    mapM_ loadScAnn scAnns
    mapM_ typeCheckScDef scDefs
  where
    typeDefs = mapMaybe (\case
                            TypeDef ss x ps t -> Just (ss, x, ps, t)
                            _ -> Nothing) ds
    scAnns = mapMaybe (\case
                          ScAnn ss x t -> Just (ss, x, t)
                          _ -> Nothing) ds
    scDefs = mapMaybe (\case
                          ScDef ss x ps e -> Just (ss, x, ps, e)
                          _ -> Nothing) ds

lookupVar :: MonadState RnTcEnv m => SrcSpan -> Id -> m (TypeScheme Id)
lookupVar ss x = do
  vm <- use variableMap
  case Map.lookup x vm of
    Just ts -> return ts
    Nothing -> typeCheckError ss $ pPrint x <+> "is not defined"

generateHeader :: (MonadState RnTcEnv m, MonadMalgo m) => Decl Id -> m ()
generateHeader TypeDef{} = pass
generateHeader (ScAnn _ x _) = do
  t <- Forall [] . TyMeta <$> newTyRef
  modify (over variableMap $ Map.insert x t)
generateHeader (ScDef _ x _ _) = do
  t <- Forall [] . TyMeta <$> newTyRef
  modify (over variableMap $ Map.insert x t)

loadTypeDef :: (MonadState RnTcEnv m, MonadMalgo m) => (SrcSpan, Id, [Id], Type Id) -> m ()
loadTypeDef (ss, x, ps, ty) = undefined

loadScAnn :: (MonadState RnTcEnv m, MonadMalgo m) => (SrcSpan, Id, TypeScheme Id) -> m ()
loadScAnn (ss, x, ts) = do
  tmp <- instantiate =<< lookupVar ss x
  ty <- instantiate ts
  unify tmp ty
  ts' <- generalize ty
  modify (over variableMap $ Map.insert x ts')

typeCheckScDef :: (MonadState RnTcEnv m, MonadMalgo m) => (SrcSpan, Id, [Id], Expr Id) -> m ()
typeCheckScDef (ss, x, ps, e) = do
  pts <- mapM (\_ -> TyMeta <$> newTyRef) ps
  modify (over variableMap (Map.fromList (zip ps (map (Forall []) pts)) <>))
  tmp <- instantiate =<< lookupVar ss x
  retType <- typeCheckExpr e
  unify tmp (funTy pts retType)
  ts <- generalize tmp
  modify (over variableMap $ Map.insert x ts)
  where
    funTy xs retType = foldr (-->) retType xs

typeCheckExpr :: (MonadState RnTcEnv m, MonadMalgo m) => Expr Id -> m (Type Id)
typeCheckExpr = undefined

generalize :: MonadMalgo m => Type Id -> m (TypeScheme Id)
generalize = undefined

instantiate :: MonadMalgo m => TypeScheme Id -> m (Type Id)
instantiate = undefined

unify :: MonadMalgo m => Type Id -> Type Id -> m ()
unify = undefined
