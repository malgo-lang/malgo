{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.FrontEnd.TypeCheck where

import           Control.Lens.TH
import qualified Data.Map.Strict                as Map
import           Language.Malgo.FrontEnd.Loc
import qualified Language.Malgo.FrontEnd.Rename as Rename
import           Language.Malgo.Id
import           Language.Malgo.IR.AST
import           Language.Malgo.Monad
import           Language.Malgo.Pretty
import           Language.Malgo.Type
import           Universum                      hiding (Type)

data LMEntry = LMEntry { _containerType :: Type, elementType :: Type }
  deriving Show

data TcGblEnv = TcGblEnv
  { _tpMap    :: Map Id TypeScheme
  , _typeMap  :: Map Id TyCon
  , _labelMap :: Map Text LMEntry
  } deriving Show

data TcLclEnv = TcLclEnv
  { _varMap   :: Map Id TypeScheme
  , _tyVarMap :: Map Id Type
  } deriving Show

-- makeLenses ''LMEntry
makeLenses ''TcGblEnv
makeLenses ''TcLclEnv

makeTcGblEnv :: Rename.RnEnv -> MalgoM TcGblEnv
makeTcGblEnv _ = return $ TcGblEnv mempty mempty mempty

type TypeCheckM a = ReaderT TcLclEnv (StateT TcGblEnv MalgoM) a

lookupVar :: (MonadState TcGblEnv m, MonadReader TcLclEnv m) => Id -> m (Maybe TypeScheme)
lookupVar name = do
  toplevel <- use tpMap
  vm <- view varMap
  return $ Map.lookup name (vm <> toplevel)

lookupTyVar :: MonadReader TcLclEnv f => Id -> f (Maybe Type)
lookupTyVar name =
  Map.lookup name <$> view tyVarMap

typeCheck :: Rename.RnEnv -> [Decl Id] -> MalgoM TcGblEnv
typeCheck rnEnv ds = do
  tcGblEnv <- makeTcGblEnv rnEnv
  executingStateT tcGblEnv
    $ usingReaderT (TcLclEnv mempty mempty)
    $ do mapM_ genHeader ds
         mapM_ checkDecl ds

-- | トップレベル宣言からヘッダーを生成してTcGblEnvに仮登録する
genHeader :: Decl Id -> TypeCheckM ()
genHeader (ScDef _ name _ _) =
  whenNothingM_ (Map.lookup name <$> use tpMap) $ do
    ty <- TyMeta <$> newTyRef
    modify (over tpMap (Map.singleton name (Forall [] ty) <>))
genHeader (ScAnn _ name _) =
  whenNothingM_ (Map.lookup name <$> use tpMap) $ do
    ty <- TyMeta <$> newTyRef
    modify (over tpMap (Map.singleton name (Forall [] ty) <>))
genHeader (TypeDef ss name xs _) = do
  tm <- use typeMap
  case Map.lookup name tm of
    Just _ -> error $ show $ "error(genHeader):" <+> pPrint ss <+> pPrint name <+> "is already defined"
    Nothing ->
      -- type List a = List a かのように環境に登録する
      modify (over typeMap (Map.singleton name (TyFun xs (TyApp (FoldedC name) (map TyVar xs))) <>))

transSType :: SType Id -> TypeCheckM Type
transSType = undefined

-- 検査する変数とinstantiateしたTypeをLclEnvに登録する
-- これにより多相再帰を防ぐ
checkDecl :: Decl Id -> TypeCheckM ()
checkDecl = undefined

checkExpr :: Expr Id -> TypeCheckM Type
checkExpr (Var ss name) = do
  mty <- lookupVar name
  case mty of
    Just ty -> instantiate ty
    Nothing -> error $ show $ "error(checkExpr):" <+> pPrint ss <+> pPrint name <+> "is not defined"
checkExpr (Literal _ lit) = return $ TyApp (checkLiteral lit) []
  where
    checkLiteral (Int _)   = IntC 64
    checkLiteral (Float _) = Float64C
    checkLiteral (Bool _)  = IntC 1
    checkLiteral (Char _)  = IntC 8
checkExpr (Record _ xs) = do
  ts <- mapM (checkExpr . view _2) xs
  return $ TyApp (RecordC (map (view _1) xs)) ts
checkExpr (Access ss e label) = do
  ety <- checkExpr e
  lm <- use labelMap
  case Map.lookup label lm of
    Just (LMEntry contTy elemTy) -> do
      unify ss ety contTy
      return elemTy
    Nothing -> do
      elemTy <- TyMeta <$> newTyRef
      modify (over labelMap
              (Map.singleton label (LMEntry ety elemTy) <>))
      return elemTy

unifyError :: SrcSpan -> Type -> Type -> TypeCheckM a
unifyError ss t1 t2 = error $ show $ "unify error(typeCheck):" <+> pPrint ss <+> text (show t1) <> "," <+> text (show t2)

-- | 型のユニフィケーションを行う
unify :: SrcSpan -> Type -> Type -> TypeCheckM ()
unify ss (TyApp c1 ts1) (TyApp c2 ts2)
  | c1 == c2 = mapM_ (uncurry $ unify ss) (zip ts1 ts2)
unify _ (TyVar name1) (TyVar name2)
  | name1 == name2 = pass
unify ss (TyMeta r1) t2 = do
  mt1 <- readTyRef r1
  case mt1 of
    Just t1 -> unify ss t1 t2
    Nothing -> writeTyRef r1 t2
unify ss t1 t2@TyMeta{} = unify ss t2 t1
unify ss t1 t2 = unifyError ss t1 t2

-- -- | 代入済みのTyMetaを再帰的に展開する
-- -- | デバッグや一致検査に利用
-- expand :: Type -> TypeCheckM Type
-- expand t@(TyMeta r) = do
--   mty <- readTyRef r
--   case mty of
--     Just ty -> expand ty
--     Nothing -> return t
-- expand (TyApp t1 t2) =
--   TyApp <$> expand t1 <*> expand t2
-- expand x = return x

-- | 代入済みのTyVarを再帰的に置換する
subst :: Type -> TypeCheckM Type
subst = undefined

generalize :: Type -> TypeCheckM TypeScheme
generalize ty = undefined
  -- rs <- collectMeta <$> expand ty
  -- vm <- view varMap
  -- ss <- concatMap collectMeta <$> mapM instantiate (elems vm)
  -- let gs = filter (`notElem` ss) rs
  -- gs' <- mapM (\(c, _) -> newTypeId (fromString [c])) (zip ['a'..] gs)
  -- mapM_ (\(r, v) -> writeTyRef r $ TyVar v) (zip gs gs')
  -- Forall gs' <$> expand ty
  -- where
  --   collectMeta (TyMeta r)  = [r]
  --   collectMeta (TyApp t1 t2) = collectMeta t1 <> collectMeta t2
  --   collectMeta _             = []

instantiate :: TypeScheme -> TypeCheckM Type
instantiate (Forall gs ty) = do
  gs' <- mapM (const $ TyMeta <$> newTyRef) gs
  local (over tyVarMap (Map.fromList (zip gs gs') <>)) $ subst ty
