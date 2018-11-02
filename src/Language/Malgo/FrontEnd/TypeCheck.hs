{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.FrontEnd.TypeCheck where

import           Control.Lens.TH
import           Data.List                       ((\\))
import qualified Data.Map.Strict                 as Map
import           Language.Malgo.FrontEnd.Loc
import           Language.Malgo.FrontEnd.RnTcEnv
import           Language.Malgo.Id
import           Language.Malgo.IR.AST
import           Language.Malgo.Monad
import           Language.Malgo.Pretty
import           Language.Malgo.Type
import           Universum                       hiding (Type)

-- スコープ内に存在するメタ変数を保持する
-- ScDefの関数と引数、letの変数、let recの関数と引数に含まれる型変数が追加され、本体部分の型検査が行われる
-- generalizeで用いる
newtype TcLclEnv = TcLclEnv { _tyMetaSet :: [TyRef Id] }

makeLenses ''TcLclEnv

typeCheckError :: SrcSpan -> Doc -> a
typeCheckError ss doc = error $ show $ "error(type check)[" <> pPrint ss <> "]" <+> doc

-- TODO: 型検査が終わった後、型環境内のすべてのTyRefに値が代入されていることを検査する
-- f :: forall a. a -> a
-- f x = xのxの型はTyMeta (TyRef (Just (TyVar "a")))となる
typeCheck :: MonadMalgo m => [Decl Id] -> m RnTcEnv
typeCheck ds = do
  env <- makeRnTcEnv
  executingStateT env $ usingReaderT (TcLclEnv []) $ do
    mapM_ generateHeader ds
    mapM_ loadTypeDef typeDefs
    mapM_ typeCheckScDef scDefs
    mapM_ loadScAnn scAnns
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

lookupTypeAlias :: MonadState RnTcEnv m => SrcSpan -> Id -> m ([Id], Type Id)
lookupTypeAlias ss x = do
  tm <- use typeAliasMap
  case Map.lookup x tm of
    Just typeAlias -> return typeAlias
    Nothing        -> typeCheckError ss $ pPrint x <+> "is not defined"

generateHeader :: (MonadIO f, MonadState RnTcEnv f) => Decl Id -> f ()
generateHeader TypeDef{} = pass
generateHeader (ScAnn _ x _) = do
  t <- Forall [] . TyMeta <$> newTyRef
  modify (over variableMap $ Map.insert x t)
generateHeader (ScDef _ x _ _) = do
  t <- Forall [] . TyMeta <$> newTyRef
  modify (over variableMap $ Map.insert x t)

loadTypeDef (ss, x, ps, ty) = undefined

loadScAnn :: (MonadState RnTcEnv m, MonadMalgo m, MonadReader TcLclEnv m) => (SrcSpan, Id, TypeScheme Id) -> m ()
loadScAnn (ss, x, typeScheme) = do
  xType <- instantiate =<< lookupVar ss x -- typeCheckScDefで推論されたxの型を取得
  unify ss xType =<< instantiate typeScheme
  typeScheme' <- generalize xType
  modify (over variableMap $ Map.insert x typeScheme')

typeCheckScDef (ss , x, ps, e) = do
  -- 引数の型を生成、環境に登録する
  pts <- mapM (\_ -> TyMeta <$> newTyRef) ps
  modify (over variableMap (Map.fromList (zip ps (map (Forall []) pts)) <>))

  retType <- TyMeta <$> newTyRef -- 返り値の型を生成

  let xType = foldr (-->) retType pts -- xの型を生成

  ms <- collectTyMeta xType  -- ここまでで生成したメタ変数のリスト

  -- 型推論を行う
  eType <- local (over tyMetaSet (ms <>)) $ typeCheckExpr e
  unify ss retType eType

  -- xを型環境に登録
  typeScheme <- generalize xType
  modify (over variableMap $ Map.insert x typeScheme)

typeCheckExpr = undefined

-- TcLclEnv.tyMetaSetに含まれないすべての空のメタ変数を型変数にする
generalize :: (MonadReader TcLclEnv m, MonadMalgo m) => Type Id -> m (TypeScheme Id)
generalize t = do
  ms <- (\\) <$> collectTyMeta t <*> view tyMetaSet
  ps <- mapM (\_ -> newId "a") ms
  mapM_ (uncurry writeTyRef) (zip ms (map TyVar ps))
  return (Forall ps t)

instantiate :: (MonadIO m, Eq a) => TypeScheme a -> m (Type a)
instantiate (Forall ps t) = do
  ms <- mapM (\_ -> TyMeta <$> newTyRef) ps
  applyType (ps, t) ms

{-
# generalizeとinstantiateの関係
ts :: TypeScheme Idとする

t <- instantiate ts
ts' <- generalize t

を実行すると、ts == ts'が成り立つ(Idの違いは無視する)
-}

unifyError :: SrcSpan -> Type Id -> Type Id -> a
unifyError ss a b = typeCheckError ss $ "cannot unify " <+> pPrint a <+> "with" <+> pPrint b

unify :: (MonadMalgo m, MonadReader TcLclEnv m, MonadState RnTcEnv m) => SrcSpan -> Type Id -> Type Id -> m ()
unify ss a@(TyVar v0) b@(TyVar v1)
  | v0 == v1 = pass
  | otherwise = unifyError ss a b
unify ss a@(TyApp (PrimC c0) xs) b@(TyApp (PrimC c1) ys)
  | c0 == c1 && length xs == length ys = mapM_ (uncurry $ unify ss) (zip xs ys)
  | otherwise = unifyError ss a b
unify ss (TyApp (SimpleC c) xs) b = do
  typeAlias <- lookupTypeAlias ss c
  a' <- applyType typeAlias xs
  unify ss a' b
unify ss a b@(TyApp (SimpleC _) _) = unify ss b a
unify ss (TyMeta r) b = do
  rVal <- readTyRef r
  case rVal of
    Just ty -> unify ss ty b
    Nothing -> writeTyRef r b
unify ss a b@(TyMeta _) = unify ss b a
unify ss a b = unifyError ss a b

-- すべての空のメタ変数を返す
-- 代入済みのメタ変数は再帰的に中身を見に行く
collectTyMeta :: MonadIO f => Type a -> f [TyRef a]
collectTyMeta (TyApp _ xs) = concatMapM collectTyMeta xs
collectTyMeta (TyVar _) = return []
collectTyMeta (TyMeta r) = do
  mt <- readTyRef r
  case mt of
    Nothing -> return [r]
    Just t  -> collectTyMeta t
