{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.FrontEnd.Rename where

import qualified Data.Map.Strict                 as Map
import           Language.Malgo.FrontEnd.Loc
import           Language.Malgo.FrontEnd.RnTcEnv
import           Language.Malgo.Id
import           Language.Malgo.IR.AST
import           Language.Malgo.Monad
import           Language.Malgo.Pretty
import           Language.Malgo.Type
import           Universum                       hiding (Type)

{-
# forallのrename
f :: forall a. a -> a
f x = let y :: a = x in y
は現在のRenamerでは処理できない．
f x = let y :: forall a. a = x in y
と書く必要がある．この式はwell-typedのはず．
-}

type RnEnv = Map Text Id

renameError :: SrcSpan -> Doc -> a
renameError ss doc = error $ show $ "error(rename)[" <> pPrint ss <> "]:" <+> doc

rename :: (MonadState RnTcEnv m, MonadMalgo m) => [Decl Text] -> m [Decl Id]
rename ds = usingReaderT Map.empty $ do
  nm <- renameTopLevel [] ds
  local (Map.fromList nm <>)
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

lookupName :: (MonadReader RnEnv m, MonadState RnTcEnv m) => SrcSpan -> Text -> m Id
lookupName ss x = do
  mx <- Map.lookup x <$> ((<>) <$> ask <*> use builtInMap)
  case mx of
    Just x' -> return x'
    Nothing -> renameError ss $ pPrint x <+> "is not defined."

renameDecl :: (MonadReader RnEnv m, MonadState RnTcEnv m, MonadMalgo m) => Decl Text -> m (Decl Id)
renameDecl (ScDef ss x ps e) = do
  x' <- lookupName ss x
  ps' <- mapM newId ps
  local (Map.fromList (zip ps ps') <>)
    $ ScDef ss x' ps' <$> renameExpr e
renameDecl (ScAnn ss x t) = do
  x' <- lookupName ss x
  ScAnn ss x' <$> renameTypeScheme t
renameDecl (TypeDef ss x ps t) = do
  x' <- lookupName ss x
  ps' <- mapM newId ps
  local (Map.fromList (zip ps ps') <>)
    $ TypeDef ss x' ps' <$> renameType t

renameExpr :: (MonadReader RnEnv m, MonadState RnTcEnv m, MonadMalgo m) => Expr Text -> m (Expr Id)
renameExpr (Var ss x) = Var ss <$> lookupName ss x
renameExpr (Literal ss x) = return $ Literal ss x
renameExpr (BinOp ss op e1 e2) = BinOp ss op <$> renameExpr e1 <*> renameExpr e2
renameExpr (If ss c t f) = If ss <$> renameExpr c <*> renameExpr t <*> renameExpr f
renameExpr (Let ss0 (NonRec ss1 x mType v) e) = do
  x' <- newId x
  mType' <- mapM renameType mType
  v' <- renameExpr v
  Let ss0 (NonRec ss1 x' mType' v')
    <$> local (Map.insert x x') (renameExpr e)
renameExpr (Let ss0 (Rec ss1 x ps mType v) e) = do
  x' <- newId x
  ps' <- mapM newId ps
  mType' <- mapM renameType mType
  v' <- local (Map.fromList ((x, x') : zip ps ps') <>) $ renameExpr v
  Let ss0 (Rec ss1 x' ps' mType' v')
    <$> local (Map.insert x x') (renameExpr e)
renameExpr (Let ss0 (TuplePat ss1 pat mType v) e) = do
  pat' <- mapM newId pat
  mType' <- mapM renameType mType
  v' <- renameExpr v
  Let ss0 (TuplePat ss1 pat' mType' v')
    <$> local (Map.fromList (zip pat pat') <>) (renameExpr e)
renameExpr (Apply ss e1 e2) =
  Apply ss <$> renameExpr e1 <*> renameExpr e2
renameExpr (Tuple ss xs) =
  Tuple ss <$> mapM renameExpr xs

renameTypeScheme :: (MonadReader RnEnv m, MonadState RnTcEnv m, MonadMalgo m) => TypeScheme Text -> m (TypeScheme Id)
renameTypeScheme (Forall xs t) = do
  xs' <- mapM newId xs
  local (Map.fromList (zip xs xs') <>)
    $ Forall xs' <$> renameType t

renameType :: (MonadReader RnEnv m, MonadState RnTcEnv m, MonadMalgo m) => Type Text -> m (Type Id)
renameType (TyApp (SimpleC name) args) = TyApp <$> (SimpleC <$> lookupName noSrcSpan name) <*> mapM renameType args
renameType (TyApp (PrimC p) args) = TyApp (PrimC p) <$> mapM renameType args
renameType (TyVar a) = TyVar <$> lookupName noSrcSpan a
renameType (TyMeta _) = renameError noSrcSpan "unreachable(renameType (TyMeta _))"
