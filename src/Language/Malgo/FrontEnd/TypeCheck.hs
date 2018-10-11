{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
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

data TcGblEnv = TcGblEnv { _tpMap    :: Map Id TypeScheme
                         , _typeMap  :: Map TypeId Type
                         , _labelMap :: Map Id LMEntry
                         }
  deriving Show

data TcLclEnv = TcLclEnv { _varMap   :: Map Id TypeScheme
                         , _tyVarMap :: Map TypeId Type
                         }
  deriving Show

makeLenses ''LMEntry
makeLenses ''TcGblEnv
makeLenses ''TcLclEnv

makeTcGblEnv :: Rename.RnEnv -> MalgoM TcGblEnv
makeTcGblEnv = undefined

type TypeCheckM a = ReaderT TcLclEnv (StateT TcGblEnv MalgoM) a

lookupVar :: (MonadState TcGblEnv m, MonadReader TcLclEnv m) => Id -> m (Maybe TypeScheme)
lookupVar name = do
  toplevel <- use tpMap
  vm <- view varMap
  return $ Map.lookup name (vm <> toplevel)

lookupTyVar :: MonadReader TcLclEnv f => TypeId -> f (Maybe Type)
lookupTyVar name =
  Map.lookup name <$> view tyVarMap

newTypeId :: Text -> TypeCheckM TypeId
newTypeId hint = TypeId <$> newId hint <*> pure Star

typeCheck :: Rename.RnEnv -> [Decl Id] -> MalgoM TcGblEnv
typeCheck rnEnv ds = do
  tcGblEnv <- makeTcGblEnv rnEnv
  executingStateT tcGblEnv
    $ usingReaderT (TcLclEnv mempty mempty)
    $ do mapM_ genHeader ds
         mapM_ checkDecl ds

-- | トップレベル宣言からヘッダーを生成してTcGblEnvに仮登録する
genHeader :: Decl Id -> TypeCheckM ()
genHeader (ScDef _ name params _) =
  whenNothingM_ (lookupVar name) $ do
    paramTys <- mapM (const $ TyMeta <$> newTyRef <*> pure Star) params
    retTy <- TyMeta <$> newTyRef <*> pure Star
    let sc = Forall [] $ makeTy paramTys retTy
    modify (over tpMap (Map.singleton name sc <>))
  where
    makeTy xs ret = foldr fn ret xs
genHeader (ScAnn ss name sty) = do
  mty <- lookupVar name
  case mty of
    Just t1 -> do
      t1' <- instantiate t1
      t2 <- transSType sty
      unify ss t1' t2
      sc <- generalize t1'
      modify (over tpMap (Map.singleton name sc <>))
    Nothing -> do
      ty <- transSType sty
      sc <- generalize ty
      modify (over tpMap (Map.singleton name sc <>))

transSType :: SType Id -> TypeCheckM Type
transSType = undefined

-- 検査する変数とinstantiateしたTypeをLclEnvに登録する
-- これにより多相再帰を防ぐ
checkDecl :: Decl Id -> TypeCheckM ()
checkDecl = undefined

-- | return value's SType is replaced with TypeScheme
checkExpr :: Expr Id -> TypeCheckM (Expr Id, TypeScheme)
checkExpr = undefined

-- | 型のユニフィケーションを行う
unify :: SrcSpan -> Type -> Type -> TypeCheckM ()
unify ss t1 t2
  | kind t1 == kind t2 = do
      t1' <- subst t1
      t2' <- subst t2
      unify' ss t1' t2'
  | otherwise = unifyError ss t1 t2

unifyError :: SrcSpan -> Type -> Type -> TypeCheckM a
unifyError ss t1 t2 = error $ show $ "unify error(typeCheck):" <+> pPrint ss <+> text (show t1) <> "," <+> text (show t2)

unify' :: SrcSpan -> Type -> Type -> TypeCheckM ()
unify' ss (TyApp t1 t2) (TyApp t3 t4) = do
  unify ss t1 t3
  unify ss t2 t4
unify' _ (TyVar name1) (TyVar name2)
  | name1 == name2 = pass
unify' _ (TyCon c1 _) (TyCon c2 _)
  | c1 == c2 = pass
unify' ss (TyMeta r1 _) t2 = do
  mt1 <- readTyRef r1
  case mt1 of
    Just t1 -> unify ss t1 t2
    Nothing -> writeTyRef r1 t2
unify' ss t1 t2@TyMeta{} = unify' ss t2 t1
unify' ss t1 t2 = unifyError ss t1 t2

-- | 代入済みのTyMetaを再帰的に展開する
-- | デバッグや一致検査に利用
expand :: Type -> TypeCheckM Type
expand = undefined

-- | 代入済みのTyVarを再帰的に置換する
subst :: Type -> TypeCheckM Type
subst = undefined

generalize :: Type -> TypeCheckM TypeScheme
generalize ty = do
  rs <- collectMeta <$> expand ty
  vm <- view varMap
  ss <- concatMap collectMeta <$> mapM instantiate (elems vm)
  let gs = filter (`notElem` ss) rs
  gs' <- mapM (\(c, _) -> newTypeId (fromString [c])) (zip ['a'..] gs)
  mapM_ (\(r, v) -> writeTyRef r $ TyVar v) (zip gs gs')
  Forall gs' <$> expand ty
  where
    collectMeta (TyMeta r _)  = [r]
    collectMeta (TyApp t1 t2) = collectMeta t1 <> collectMeta t2
    collectMeta _             = []

instantiate :: TypeScheme -> TypeCheckM Type
instantiate (Forall gs ty) = do
  gs' <- mapM (const $ TyMeta <$> newTyRef <*> pure Star) gs
  local (over tyVarMap (Map.fromList (zip gs gs') <>)) $ subst ty
