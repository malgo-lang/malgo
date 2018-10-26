{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeFamilies     #-}
module Language.Malgo.FrontEnd.Rename where

import           Control.Lens.TH
import qualified Data.List as List
import qualified Data.Map.Strict                 as Map
import           Data.Outputable
import           Language.Malgo.FrontEnd.Loc
import           Language.Malgo.FrontEnd.RnTcEnv
import           Language.Malgo.Id
import           Language.Malgo.IR.AST
import           Language.Malgo.Monad
import           Language.Malgo.Pretty
import           Language.Malgo.Type
import           Universum                       hiding (Type)

data RnEnv = RnEnv
  { _rnTcEnv :: RnTcEnv
  , _nameMap :: Map Text Id
  } deriving (Show, Generic)

makeLenses ''RnEnv

instance Outputable RnEnv

renameError :: SrcSpan -> Doc -> a
renameError ss doc = error $ show $ "error(rename)[" <> pPrint ss <> "]:" <+> doc

rename :: RnTcEnv -> [Decl Text] -> MalgoM [Decl Id]
rename rte ds = usingReaderT (RnEnv rte Map.empty) $ do
  nm <- renameTopLevel [] ds
  local (over nameMap (Map.fromList nm <>))
    $ mapM renameDecl ds

renameTopLevel :: MonadMalgo m => [(Text, Id)] -> [Decl Text] -> m [(Text, Id)]
renameTopLevel xs [] = return xs
renameTopLevel xs (ScDef _ x _ _ : ds)
  | x `elem` map (view _1) xs = renameTopLevel xs ds
  | otherwise = do
      x' <- newId x
      renameTopLevel ((x, x') : xs) ds
renameTopLevel xs (ScAnn ss x _ : ds)
  | x `elem` map (view _1) xs = renameError ss $ pPrint x <+> "is already defined."
  | otherwise = do
      x' <- newId x
      renameTopLevel ((x, x') : xs) ds
renameTopLevel xs (TypeDef ss x _ _ : ds)
  | x `elem` map (view _1) xs = renameError ss $ pPrint x <+> "is already defined."
  | otherwise = do
      x' <- newId x
      renameTopLevel ((x, x') : xs) ds

lookupName :: MonadReader RnEnv m => SrcSpan -> Text -> m Id
lookupName ss x = do
  mx <- Map.lookup x <$> ((<>) <$> view nameMap
                          <*> view (rnTcEnv . builtInMap))
  case mx of
    Just x' -> return x'
    Nothing -> renameError ss $ pPrint x <+> "is not defined."

renameDecl :: (MonadReader RnEnv m, MonadMalgo m) => Decl Text -> m (Decl Id)
renameDecl (ScDef ss x ps e) = do
  x' <- lookupName ss x
  ps' <- mapM newId ps
  local (over nameMap (Map.fromList (zip (ps :: [Text]) ps') <>))
    $ ScDef ss x' ps' <$> renameExpr e
renameDecl (ScAnn ss x t) = do
  x' <- lookupName ss x
  ScAnn ss x' <$> renameTypeScheme t
renameDecl (TypeDef ss x ps t) = do
  x' <- lookupName ss x
  ps' <- mapM newId ps
  local (over nameMap (Map.fromList (zip ps ps') <>))
    $ TypeDef ss x' ps' <$> renameType t

renameExpr :: (MonadReader RnEnv m, MonadMalgo m) => Expr Text -> m (Expr Id)
renameExpr (Var ss x) = Var ss <$> lookupName ss x
renameExpr (Literal ss x) = return $ Literal ss x
renameExpr (BinOp ss op e1 e2) = BinOp ss op <$> renameExpr e1 <*> renameExpr e2
renameExpr (If ss c t f) = If ss <$> renameExpr c <*> renameExpr t <*> renameExpr f
renameExpr (Let ss0 (NonRec ss1 x mt v) e) = do
  x' <- newId x
  mt' <- mapM renameType mt
  v' <- renameExpr v
  Let ss0 (NonRec ss1 x' mt' v')
    <$> local (over nameMap (Map.insert x x')) (renameExpr e)
renameExpr (Let ss0 (Rec ss1 x ps mt v) e) = do
  x' <- newId x
  ps' <- mapM (\(p, t) -> (,) <$> newId p <*> mapM renameType t) ps
  mt' <- mapM renameType mt
  v' <- local (over nameMap (Map.fromList ((x, x') : zip (map (view _1) ps) (map (view _1) ps')) <>)) $ renameExpr v
  Let ss0 (Rec ss1 x' ps' mt' v')
    <$> local (over nameMap (Map.insert x x')) (renameExpr e)
renameExpr (Apply ss e1 e2) =
  Apply ss <$> renameExpr e1 <*> renameExpr e2
renameExpr (Tuple ss xs) =
  Tuple ss <$> mapM renameExpr xs
renameExpr (Access ss e i) =
  Access ss <$> renameExpr e <*> pure i

renameTypeScheme :: (MonadReader RnEnv m, MonadMalgo m) => TypeScheme Text -> m (TypeScheme Id)
renameTypeScheme (Forall xs t) = do
  xs' <- mapM newId xs
  local (over nameMap (Map.fromList (zip xs xs') <>))
    $ Forall xs' <$> renameType t

renameType :: (MonadReader RnEnv m, MonadMalgo m) => Type Text -> m (Type Id)
renameType = undefined
