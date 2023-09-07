-- | Resolve name conflicts and desugar some syntax.
module Malgo.Rename (rename) where

import Control.Monad.Reader (MonadReader (ask, local), runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Cont (ContT (..), shiftT)
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import Malgo.Monad
import Malgo.Prelude
import Malgo.Syntax

rename :: (MonadMalgo m) => Expr Text -> m (Expr Id)
rename e = do
  env <- newRnEnv
  runReaderT (rnExpr e) env

newtype RnEnv = RnEnv
  { nameMap :: Map Text Id
  }

newRnEnv :: (Applicative m) => m RnEnv
newRnEnv = pure (RnEnv mempty)

lookupName :: (MonadMalgo m, MonadReader RnEnv m) => Text -> m Id
lookupName name = do
  env <- ask
  case Map.lookup name env.nameMap of
    Just id -> pure id
    Nothing -> error $ "not defined: " <> show name

withNewNames :: (MonadMalgo m, MonadReader RnEnv m) => [Text] -> m a -> m a
withNewNames names k = do
  ids <- traverse newId names
  local (\env -> env {nameMap = Map.union (Map.fromList $ zip names ids) env.nameMap}) k

rnExpr :: (MonadMalgo m, MonadReader RnEnv m) => Expr Text -> m (Expr Id)
rnExpr (Var p name) = Var p <$> lookupName name
rnExpr (Lit p a) = pure $ Lit p a
rnExpr (App f args) = App <$> rnExpr f <*> traverse rnExpr args
rnExpr (Codata p clauses) = Codata p <$> traverse rnClause clauses

rnClause :: (MonadMalgo m, MonadReader RnEnv m) => Clause Text -> m (Clause Id)
rnClause (Clause pat body) = runContT (rnPat pat) \pat' -> do
  Clause pat' <$> rnExpr body

rnPat :: (MonadMalgo m, MonadReader RnEnv m) => Pat Text -> ContT r m (Pat Id)
rnPat (PThis p) = pure $ PThis p
rnPat (PVar p name) = shiftT \k ->
  withNewNames [name] do
    name' <- lookupName name
    lift $ k (PVar p name')
rnPat (PLit p a) = pure $ PLit p a
rnPat (PApp f args) = PApp <$> rnPat f <*> traverse rnPat args