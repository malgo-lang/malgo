{-# LANGUAGE TypeFamilies #-}

module Malgo.Syntax.ResolveName (resolveName, ResolveError) where

import Data.Map qualified as Map
import Data.Traversable (for)
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader, ask, local, runReader)
import Malgo.Location (Location)
import Malgo.Name (Name, newName)
import Malgo.Prelude
import Malgo.Syntax
import Malgo.Unique

resolveName :: (UniqueGen :> es, Error ResolveError :> es) => [Definition Text] -> Eff es [Definition Name]
resolveName definitions = runReader @Env mempty do
  -- generate toplevel environment
  toplevel <- for definitions \Definition {name} -> do
    name' <- newName name
    pure (name, name')
  with toplevel do
    traverse resolve definitions

newtype Env = Env
  { nameMap :: Map Text Name
  }
  deriving (Semigroup, Monoid)

with :: (Reader Env :> es) => [(Text, Name)] -> Eff es a -> Eff es a
with kvs action = do
  local (\env -> Env {nameMap = Map.fromList kvs <> env.nameMap}) action

lookup :: (Reader Env :> es, Error ResolveError :> es) => Location -> Text -> Eff es Name
lookup location name = do
  env <- ask @Env
  case Map.lookup name env.nameMap of
    Just name -> pure name
    Nothing -> throwError $ UnboundVariable location name

data ResolveError
  = UnboundVariable
  { location :: Location,
    name :: Text
  }
  deriving (Show)

class Resolve a r where
  resolve :: a -> r

instance (UniqueGen :> es, Reader Env :> es, Error ResolveError :> es) => Resolve (Definition Text) (Eff es (Definition Name)) where
  resolve Definition {..} = do
    name' <- lookup location name
    paramsKvs <- for params \param -> do
      param' <- newName param
      pure (param, param')
    returnsKvs <- for returns \ret -> do
      ret' <- newName ret
      pure (ret, ret')
    with paramsKvs do
      with returnsKvs do
        term' <- resolve term
        pure
          Definition
            { location,
              name = name',
              params = map snd paramsKvs,
              returns = map snd returnsKvs,
              term = term'
            }

instance (UniqueGen :> es, Reader Env :> es, Error ResolveError :> es) => Resolve (Term Text) (Eff es (Term Name)) where
  resolve Var {..} = do
    name' <- lookup location name
    pure Var {location, name = name'}
  resolve Literal {..} = pure Literal {location, literal}
  resolve Construct {..} = do
    producers' <- traverse resolve producers
    consumers' <- traverse resolve consumers
    pure Construct {location, tag, producers = producers', consumers = consumers'}
  resolve Comatch {..} = do
    coclauses' <- traverse resolve coclauses
    pure Comatch {location, coclauses = coclauses'}
  resolve Destruct {..} = do
    term' <- resolve term
    producers' <- traverse resolve producers
    consumers' <- traverse resolve consumers
    pure Destruct {location, term = term', tag, producers = producers', consumers = consumers'}
  resolve Match {..} = do
    term' <- resolve term
    clauses' <- traverse resolve clauses
    pure Match {location, term = term', clauses = clauses'}
  resolve Prim {..} = do
    producers' <- traverse resolve producers
    consumers' <- traverse resolve consumers
    pure Prim {location, tag, producers = producers', consumers = consumers'}
  resolve Switch {..} = do
    term' <- resolve term
    branches' <- for branches \(literal, term) -> do
      term' <- resolve term
      pure (literal, term')
    defaultBranch' <- resolve defaultBranch
    pure Switch {location, term = term', branches = branches', defaultBranch = defaultBranch'}
  resolve Invoke {..} = do
    name' <- lookup location name
    producers' <- traverse resolve producers
    consumers' <- traverse resolve consumers
    pure Invoke {location, name = name', producers = producers', consumers = consumers'}
  resolve Label {..} = do
    name' <- newName name
    with [(name, name')] do
      term' <- resolve term
      pure Label {location, name = name', term = term'}
  resolve Goto {..} = do
    name' <- lookup location name
    term' <- resolve term
    pure Goto {location, name = name', term = term'}

instance (UniqueGen :> es, Reader Env :> es, Error ResolveError :> es) => Resolve (Coclause Text) (Eff es (Coclause Name)) where
  resolve Coclause {..} = do
    (copattern', copatternKvs) <- resolveCopattern copattern
    with copatternKvs do
      term' <- resolve term
      pure Coclause {copattern = copattern', term = term'}

resolveCopattern :: (UniqueGen :> es) => Copattern Text -> Eff es (Copattern Name, [(Text, Name)])
resolveCopattern Copattern {..} = do
  params' <- traverse newName params
  returns' <- traverse newName returns
  pure
    $ ( Copattern
          { tag,
            params = params',
            returns = returns'
          },
        zip params params' <> zip returns returns'
      )

instance (UniqueGen :> es, Reader Env :> es, Error ResolveError :> es) => Resolve (Clause Text) (Eff es (Clause Name)) where
  resolve Clause {..} = do
    (pattern', patternKvs) <- resolve pattern
    with patternKvs do
      term' <- resolve term
      pure Clause {pattern = pattern', term = term'}

instance (UniqueGen :> es) => Resolve (Pattern Text) (Eff es (Pattern Name, [(Text, Name)])) where
  resolve PConstruct {..} = do
    (params', paramsKvs) <- unzip <$> traverse resolve params
    (returns', returnsKvs) <- unzip <$> traverse resolve returns
    pure
      ( PConstruct
          { tag,
            params = params',
            returns = returns'
          },
        concat paramsKvs <> concat returnsKvs
      )
  resolve PVar {..} = do
    name' <- newName name
    pure (PVar {name = name'}, [(name, name')])