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
-- f :: forall a. a
-- f = f
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

loadTypeDef :: MonadState RnTcEnv m => (SrcSpan, Id, [Id], Type Id) -> m ()
loadTypeDef (_, x, ps, ty) =
  modify (over typeAliasMap $ Map.insert x (ps, ty))

loadScAnn :: (MonadState RnTcEnv m, MonadMalgo m, MonadReader TcLclEnv m) => (SrcSpan, Id, TypeScheme Id) -> m ()
loadScAnn (ss, x, typeScheme) = do
  xType <- instantiate =<< lookupVar ss x -- typeCheckScDefで推論されたxの型を取得
  unify ss xType =<< instantiate typeScheme
  typeScheme' <- generalize xType
  modify (over variableMap $ Map.insert x typeScheme')

typeCheckScDef :: (MonadState RnTcEnv m, MonadReader TcLclEnv m, MonadMalgo m) => (SrcSpan, Id, [Id], Expr Id) -> m ()
typeCheckScDef (ss , x, ps, e) = do
  -- 引数の型を生成、環境に登録する
  pts <- mapM (\_ -> TyMeta <$> newTyRef) ps
  modify (over variableMap (Map.fromList (zip ps (map (Forall []) pts)) <>))

  retType <- TyMeta <$> newTyRef -- 返り値の型を生成

  let xType = foldr (-->) retType pts -- xの型を生成

  ms <- collectTyMeta xType -- ここまでで生成したメタ変数のリスト

  -- 型推論を行う
  eType <- local (over tyMetaSet (ms <>)) $ typeCheckExpr e
  unify ss retType eType

  -- xを型環境に登録
  typeScheme <- generalize xType
  modify (over variableMap $ Map.insert x typeScheme)

typeCheckExpr :: (MonadState RnTcEnv m, MonadMalgo m, MonadReader TcLclEnv m) => Expr Id -> m (Type Id)
typeCheckExpr (Var ss x) = instantiate =<< lookupVar ss x
typeCheckExpr (Literal _ (Int _)) = return intType
typeCheckExpr (Literal _ (Float _)) = return doubleType
typeCheckExpr (Literal _ (Bool _)) = return boolType
typeCheckExpr (Literal _ (Char _)) = return charType
typeCheckExpr (Literal _ (String _)) = return stringType
typeCheckExpr (BinOp ss op x y) = do
  xType <- typeCheckExpr x
  yType <- typeCheckExpr y
  (left, right, result) <- typeCheckOp op
  unify ss left xType
  unify ss right yType
  return result
typeCheckExpr (If ss c t f) = do
  cType <- typeCheckExpr c
  unify ss cType boolType

  tType <- typeCheckExpr t
  fType <- typeCheckExpr f
  unify ss tType fType

  return tType
typeCheckExpr (Let _ (NonRec ss x mTypeScheme v) e) = do
  vType <- typeCheckExpr v
  case mTypeScheme of
    Just (Forall [] xType) ->
      unify ss xType vType
    Just typeScheme
      | isSyntactic v -> unify ss vType =<< instantiate typeScheme
      | otherwise -> typeCheckError ss $ "type annotation" <+> pPrint typeScheme <+> "cannot have `forall`"
    Nothing -> pass
  typeScheme <- if isSyntactic v
                then generalize vType
                else pure $ Forall [] vType
  modify (over variableMap $ Map.insert x typeScheme)
  ms <- collectTyMeta vType
  local (over tyMetaSet (ms <>)) $ typeCheckExpr e
typeCheckExpr (Let _ (TuplePat ss pat mTypeScheme v) e) = do
  vType <- typeCheckExpr v
  case mTypeScheme of
    Just (Forall [] xType) ->
      unify ss xType vType
    Just typeScheme
      | isSyntactic v -> unify ss vType =<< instantiate typeScheme
      | otherwise -> typeCheckError ss $ "type annotation" <+> pPrint typeScheme <+> "cannot have `forall`"
    Nothing -> pass
  patTypes <- mapM (const $ TyMeta <$> newTyRef) pat
  let patType = tupleType patTypes
  unify ss vType patType

  patTypeSchemes <- if isSyntactic v
                    then mapM generalize patTypes
                    else pure $ map (Forall []) patTypes
  modify (over variableMap (Map.fromList (zip pat patTypeSchemes) <>))
  ms <- collectTyMeta patType
  local (over tyMetaSet (ms <>)) $ typeCheckExpr e
typeCheckExpr (Let _ (Rec ss f xs mTypeScheme v) e) = do
  xsTypes <- mapM (const $ TyMeta <$> newTyRef) xs
  retType <- TyMeta <$> newTyRef
  let fType = foldr (-->) retType xsTypes

  modify (over variableMap (Map.fromList ((f, Forall [] fType) : zip xs (map (Forall []) xsTypes)) <>))

  ms0 <- collectTyMeta fType
  vType <- local (over tyMetaSet (ms0 <>)) $ typeCheckExpr v

  unify ss retType vType

  case mTypeScheme of
    Just typeScheme -> do
      annType <- instantiate typeScheme
      unify ss fType annType
    Nothing -> pass

  typeScheme <- generalize fType
  modify (over variableMap (Map.insert f typeScheme))

  ms1 <- collectTyMeta fType
  local (over tyMetaSet (ms1 <>)) $ typeCheckExpr e
typeCheckExpr (Apply ss f x) = do
  retType <- TyMeta <$> newTyRef
  xType <- typeCheckExpr x
  fType <- typeCheckExpr f
  unify ss fType (xType --> retType)
  return retType
typeCheckExpr (Tuple _ xs) = do
  xsTypes <- mapM typeCheckExpr xs
  return $ tupleType xsTypes
-- typeCheckExpr (Access ss e i) = do
--   eType <- typeCheckExpr e
--   eType' <- expandType eType
--   case eType' of
--     (TyApp (PrimC (TupleC n)) ts)
--       | i <= n -> return $ ts !! i
--       | otherwise -> typeCheckError ss "out of bounds"
--     _ -> typeCheckError ss "infered type is not tuple"
--   where
--     expandType (TyMeta r) = do
--       rVal <- readTyRef r
--       case rVal of
--         Nothing -> typeCheckError ss "cannot infer type"
--         Just (TyMeta r') -> expandType (TyMeta r')
--         Just t -> return t
--     expandType t = return t

typeCheckOp :: MonadIO m => Op -> m (Type a, Type a, Type a)
typeCheckOp Add = return (intType, intType, intType)
typeCheckOp Sub = return (intType, intType, intType)
typeCheckOp Mul = return (intType, intType, intType)
typeCheckOp Div = return (intType, intType, intType)
typeCheckOp Mod = return (intType, intType, intType)
typeCheckOp FAdd = return (doubleType, doubleType, doubleType)
typeCheckOp FSub = return (doubleType, doubleType, doubleType)
typeCheckOp FMul = return (doubleType, doubleType, doubleType)
typeCheckOp FDiv = return (doubleType, doubleType, doubleType)
-- Eq, Neq, Lt, Gt, Le, Geはすべての型で実行可能とする
typeCheckOp Eq = do
  t <- TyMeta <$> newTyRef
  return (t, t, boolType)
typeCheckOp Neq = do
  t <- TyMeta <$> newTyRef
  return (t, t, boolType)
typeCheckOp Lt = do
  t <- TyMeta <$> newTyRef
  return (t, t, boolType)
typeCheckOp Gt = do
  t <- TyMeta <$> newTyRef
  return (t, t, boolType)
typeCheckOp Le = do
  t <- TyMeta <$> newTyRef
  return (t, t, boolType)
typeCheckOp Ge = do
  t <- TyMeta <$> newTyRef
  return (t, t, boolType)
typeCheckOp And = return (boolType, boolType, boolType)
typeCheckOp Or = return (boolType, boolType, boolType)

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

-- TODO: unifyをEither辺りで包んで、複数の候補のうち最初に該当したものにunifyできるようにする
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
  isOccur <- occur r b
  if isOccur
    then unifyError ss (TyMeta r) b
    else do rVal <- readTyRef r
            case rVal of
              Just ty -> unify ss ty b
              Nothing -> writeTyRef r b
unify ss a b@(TyMeta _) = unify ss b a
unify ss a b = unifyError ss a b

occur :: MonadIO m => TyRef a -> Type a -> m Bool
occur _ (TyVar _) = return False
occur r0 (TyApp _ ts) = allM (occur r0) ts
occur r0 (TyMeta r1)
  | r0 == r1 = return True
  | otherwise = do
      r1Val <- readTyRef r1
      case r1Val of
        Nothing -> return False
        Just t  -> occur r0 t

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

isSyntactic :: Expr a -> Bool
isSyntactic Var{}        = True
isSyntactic Literal{}    = True
isSyntactic (Tuple _ xs) = all isSyntactic xs
isSyntactic _            = False
