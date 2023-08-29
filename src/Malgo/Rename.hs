-- | Resolve name conflicts and desugar some syntax.
module Malgo.Rename (rename) where

import Control.Monad.Reader (MonadReader (ask, local), ReaderT, runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Cont (ContT (..), shiftT)
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Malgo.Monad
import Malgo.Prelude
import Malgo.Syntax

rename :: Expr Text -> MalgoM (Expr Id)
rename e = do
  env <- newRnEnv
  runReaderT (rnExpr e) env

data Id = Id {name :: Text, uniq :: Int}
  deriving stock (Eq, Ord, Show, Generic)

newtype RnEnv = RnEnv
  {nameMap :: Map Text Id}

newRnEnv :: MalgoM RnEnv
newRnEnv = pure (RnEnv mempty)

type RenameM = ReaderT RnEnv MalgoM

lookupName :: Text -> RenameM Id
lookupName name = do
  env <- ask
  case Map.lookup name env.nameMap of
    Just id -> pure id
    Nothing -> error $ "not defined: " <> show name

internName :: Text -> RenameM Id
internName name = do
  uniq <- lift newUniq
  pure $ Id name uniq

withNewNames :: [Text] -> ([Id] -> RenameM a) -> RenameM a
withNewNames names k = do
  ids <- traverse internName names
  local (\env -> env {nameMap = Map.union (Map.fromList $ zip names ids) env.nameMap}) (k ids)

rnExpr :: Expr Text -> RenameM (Expr Id)
rnExpr (Var name) = Var <$> lookupName name
rnExpr (Lit a) = pure $ Lit a
rnExpr (App f args) = App <$> rnExpr f <*> traverse rnExpr args
rnExpr (Codata clauses) = Codata <$> traverse rnClause clauses

rnClause :: Clause Text -> RenameM (Clause Id)
rnClause (Clause pat body) = runContT (rnPat pat) \pat' -> do
  Clause pat' <$> rnExpr body

rnPat :: Pat Text -> ContT a RenameM (Pat Id)
rnPat PThis = pure PThis
rnPat (PVar name) = shiftT \k ->
  lift $ withNewNames [name] \case
    [name'] -> k (PVar name')
    _ -> error "impossible"
rnPat (PLit a) = pure $ PLit a
rnPat (PApp f args) = PApp <$> rnPat f <*> traverse rnPat args