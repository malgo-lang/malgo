{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.FrontEnd.TypeCheck where

import           Control.Lens.TH
import qualified Data.Map.Strict                as Map
import qualified Language.Malgo.FrontEnd.Rename as Rename
import           Language.Malgo.Id
import           Language.Malgo.IR.AST
import           Language.Malgo.Monad
import           Language.Malgo.Type
import           Universum                      hiding (Type)

-- type LabelMap = Map Text (Type, Type)

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

makeLenses ''TcGblEnv
makeLenses ''TcLclEnv

makeTcGblEnv :: Rename.RnEnv -> MalgoM TcGblEnv
makeTcGblEnv = undefined

type TypeCheckM a = StateT TcGblEnv (ReaderT TcLclEnv MalgoM) a

lookupVar :: (MonadState TcGblEnv m, MonadReader TcLclEnv m) => Id -> m (Maybe TypeScheme)
lookupVar name = do
  toplevel <- use tpMap
  vm <- view varMap
  return $ Map.lookup name (vm <> toplevel)

lookupTyVar :: MonadReader TcLclEnv f => TypeId -> f (Maybe Type)
lookupTyVar name =
  Map.lookup name <$> view tyVarMap

newMetaVar :: TypeCheckM TyRef
newMetaVar = TyRef <$> newIORef Nothing

newTypeId :: Text -> TypeCheckM TypeId
newTypeId hint = TypeId <$> newId hint <*> pure Star

typeCheck :: [Decl Id] -> MalgoM TcGblEnv
typeCheck = undefined

-- | トップレベル宣言からヘッダーを生成してTcGblEnvに仮登録する
genHeader :: Decl Id -> TypeCheckM ()
genHeader (ScDef _ name params _) =
  whenNothingM_ (lookupVar name) $ do
    paramTys <- mapM (const $ TyMeta <$> newMetaVar <*> pure Star) params
    retTy <- TyMeta <$> newMetaVar <*> pure Star
    let sc = Forall [] $ makeTy paramTys retTy
    modify (over tpMap (Map.singleton name sc <>))
  where
    makeTy xs ret = foldr fn ret xs
genHeader (ScAnn _ name sty) = do
  mty <- lookupVar name
  case mty of
    Just t1 -> do
      t1' <- instantiate t1
      t2 <- transSType sty
      unify t1' t2
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
unify :: Type -> Type -> TypeCheckM ()
unify = undefined

-- | 代入済みのTyMetaを展開する
expand :: Type -> TypeCheckM Type
expand = undefined

subst :: Type -> TypeCheckM Type
subst = undefined

generalize :: Type -> TypeCheckM TypeScheme
generalize ty = do
  rs <- collectMeta <$> expand ty
  vm <- view varMap
  ss <- concatMap collectMeta <$> mapM instantiate (elems vm)
  let gs = filter (`notElem` ss) rs
  gs' <- mapM (\(c, _) -> newTypeId (fromString [c])) (zip ['a'..] gs)
  mapM_ (\(TyRef r, v) -> writeIORef r $ Just $ TyVar v) (zip gs gs')
  Forall gs' <$> expand ty
  where
    collectMeta (TyMeta r _)  = [r]
    collectMeta (TyApp t1 t2) = collectMeta t1 <> collectMeta t2
    collectMeta _             = []

instantiate :: TypeScheme -> TypeCheckM Type
instantiate (Forall gs ty) = do
  gs' <- mapM (const $ TyMeta <$> newMetaVar <*> pure Star) gs
  local (over tyVarMap (Map.fromList (zip gs gs') <>)) $ subst ty
