-- | Resolve name conflicts and desugar some syntax.
module Malgo.Rename (rename) where

import Control.Monad.Reader (MonadReader (ask, local), ReaderT, asks, runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Cont (ContT (..), shiftT)
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import Malgo.Monad
import Malgo.Prelude
import Malgo.Syntax

rename :: FilePath -> Expr Text -> MalgoM (Expr Id)
rename sourceFilePath e = do
  env <- newRnEnv sourceFilePath
  runReaderT (rnExpr e) env

data RnEnv = RnEnv
  { nameMap :: Map Text Id,
    sourceFilePath :: FilePath
  }

newRnEnv :: FilePath -> MalgoM RnEnv
newRnEnv sourceFilePath = pure (RnEnv mempty sourceFilePath)

type RenameM = ReaderT RnEnv MalgoM

lookupName :: (MonadMalgo m, MonadReader RnEnv m) => Text -> m Id
lookupName name = do
  env <- ask
  case Map.lookup name env.nameMap of
    Just id -> pure id
    Nothing -> error $ "not defined: " <> show name

withNewNames :: (MonadMalgo m, MonadReader RnEnv m) => [Text] -> m a -> m a
withNewNames names k = do
  sourceFilePath <- asks (.sourceFilePath)
  ids <- traverse (newId sourceFilePath) names
  local (\env -> env {nameMap = Map.union (Map.fromList $ zip names ids) env.nameMap}) k

rnExpr :: Expr Text -> RenameM (Expr Id)
rnExpr (Var p name) = Var p <$> lookupName name
rnExpr (Lit p a) = pure $ Lit p a
rnExpr (App f args) = App <$> rnExpr f <*> traverse rnExpr args
rnExpr (Codata p clauses) = Codata p <$> traverse rnClause clauses

rnClause :: Clause Text -> RenameM (Clause Id)
rnClause (Clause pat body) = runContT (rnPat pat) \pat' -> do
  Clause pat' <$> rnExpr body

rnPat :: Pat Text -> ContT a RenameM (Pat Id)
rnPat (PThis p) = pure $ PThis p
rnPat (PVar p name) = shiftT \k ->
  withNewNames [name] do
    name' <- lookupName name
    lift $ k (PVar p name')
rnPat (PLit p a) = pure $ PLit p a
rnPat (PApp f args) = PApp <$> rnPat f <*> traverse rnPat args