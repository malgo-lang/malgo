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
newtype TcLclEnv = TcLclEnv { _tyMetaSet :: [TyRef (Type Id)] }

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

lookupVar ss x = do
  vm <- use variableMap
  case Map.lookup x vm of
    Just ts -> return ts
    Nothing -> typeCheckError ss $ pPrint x <+> "is not defined"

generateHeader TypeDef{} = pass
generateHeader (ScAnn _ x _) = do
  t <- Forall [] . TyMeta <$> newTyRef
  modify (over variableMap $ Map.insert x t)
generateHeader (ScDef _ x _ _) = do
  t <- Forall [] . TyMeta <$> newTyRef
  modify (over variableMap $ Map.insert x t)

loadTypeDef (ss, x, ps, ty) = undefined

loadScAnn (ss, x, ts) = do
  tmp <- instantiate =<< lookupVar ss x
  ty <- instantiate ts
  unify tmp ty
  ts' <- generalize ty
  modify (over variableMap $ Map.insert x ts')

typeCheckScDef (ss, x, ps, e) = do
  pts <- mapM (\_ -> TyMeta <$> newTyRef) ps
  modify (over variableMap (Map.fromList (zip ps (map (Forall []) pts)) <>))
  tmp <- instantiate =<< lookupVar ss x
  ms <- mapM collectTyMeta (tmp : pts)
  retType <- local (over tyMetaSet (ms <>)) $ typeCheckExpr e
  unify tmp (funTy pts retType)
  ts <- generalize tmp
  modify (over variableMap $ Map.insert x ts)
  where
    funTy xs retType = foldr (-->) retType xs

typeCheckExpr = undefined

-- TcLclEnv.tyMetaSetに含まれないすべての空のメタ変数を型変数にする
generalize = undefined

instantiate = undefined

{-
# generalizeとinstantiateの関係
ts :: TypeScheme Idとする

t <- instantiate ts
ts' <- generalize t

を実行すると、ts == ts'が成り立つ(Idの違いは無視する)
-}

unify = undefined

-- すべての空のメタ変数を返す
-- 代入済みのメタ変数は再帰的に中身を見に行く
collectTyMeta = undefined
