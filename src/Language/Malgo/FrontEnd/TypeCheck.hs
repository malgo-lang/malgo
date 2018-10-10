{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Language.Malgo.FrontEnd.TypeCheck where

import qualified Data.Map.Strict as Map
import           Control.Lens.TH
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

data TcLclEnv = TcLclEnv { _varMap :: Map Id TypeScheme
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

typeCheck :: [Decl Id] -> MalgoM TcGblEnv
typeCheck = undefined

-- | トップレベル宣言からヘッダーを生成してTcGblEnvに仮登録する
genHeader :: Decl Id -> TypeCheckM ()
genHeader (ScDef _ name params _) =
  whenNothingM_ (lookupVar name) $ do
    paramTys <- mapM (const $ TyMeta <$> newMetaVar <*> pure Type) params
    retTy <- TyMeta <$> newMetaVar <*> pure Type
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

checkDecl :: Decl Id -> TypeCheckM ()
checkDecl = undefined

checkExpr :: Expr Id -> TypeCheckM TypeScheme
checkExpr = undefined

-- | 型のユニフィケーションを行う
unify :: Type -> Type -> TypeCheckM ()
unify = undefined

-- | 代入済みのTyMetaを展開する
expand :: Type -> TypeCheckM Type
expand = undefined

generalize :: Type -> TypeCheckM TypeScheme
generalize = undefined

instantiate :: TypeScheme -> TypeCheckM Type
instantiate = undefined
