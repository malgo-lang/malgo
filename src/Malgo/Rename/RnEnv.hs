{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | 'Malgo.Rename.RnEnv' contains functions which convert 'PsId' to 'RnId'.
module Malgo.Rename.RnEnv where

import Control.Lens (ASetter', At (at), Lens', lens, makeFieldsNoPrefix, over, view, (^.))
import Data.HashMap.Strict qualified as HashMap
import Koriel.Id
import Koriel.Lens
import Koriel.MonadUniq
import Koriel.Pretty
import {-# SOURCE #-} Malgo.Interface
import Malgo.Prelude
import Malgo.Syntax.Extension

data RnState = RnState
  { _infixInfo :: HashMap RnId (Assoc, Int),
    _dependencies :: [ModuleName]
  }
  deriving stock (Show)

instance Pretty RnState where
  pPrint RnState {_infixInfo, _dependencies} =
    "RnState"
      <+> braces
        ( sep
            [ sep ["_infixInfo", "=", pPrint $ HashMap.toList _infixInfo],
              sep ["_dependencies", "=", pPrint _dependencies]
            ]
        )

infixInfo :: Lens' RnState (HashMap RnId (Assoc, Int))
infixInfo = lens (._infixInfo) (\r x -> r {_infixInfo = x})

dependencies :: Lens' RnState [ModuleName]
dependencies = lens (._dependencies) (\r x -> r {_dependencies = x})

-- | Resolved identifier
type Resolved = Qualified RnId

data RnEnv = RnEnv
  { -- | Environment for resolved variable identifiers.
    -- The key is the raw identifier (e.g. `foo`, `bar`).
    -- The value is the list of resolved identifiers (e.g. `foo`, `Foo.foo`, `B.bar`).
    _resolvedVarIdentMap :: HashMap PsId [Resolved],
    _resolvedTypeIdentMap :: HashMap PsId [Resolved],
    moduleName :: ModuleName,
    _uniqSupply :: UniqSupply,
    _srcName :: FilePath,
    _debugMode :: Bool,
    _modulePaths :: [FilePath],
    _interfaces :: IORef (HashMap ModuleName Interface)
  }

makeFieldsNoPrefix ''RnEnv

-- | Append resolved identifiers to the environment.
appendRnEnv :: ASetter' RnEnv (HashMap PsId [Resolved]) -> [(PsId, Resolved)] -> RnEnv -> RnEnv
appendRnEnv lens newEnv = over lens $
  \e -> foldr (\(k, v) -> HashMap.insertWith (<>) k [v]) e newEnv

-- | Generate RnId of primitive types
genBuiltinRnEnv :: (MonadReader env m, HasUniqSupply env UniqSupply, MonadIO m) => ModuleName -> MalgoEnv -> m RnEnv
genBuiltinRnEnv modName malgoEnv = do
  int32_t <- newExternalId "Int32#" () $ ModuleName "Builtin"
  int64_t <- newExternalId "Int64#" () $ ModuleName "Builtin"
  float_t <- newExternalId "Float#" () $ ModuleName "Builtin"
  double_t <- newExternalId "Double#" () $ ModuleName "Builtin"
  char_t <- newExternalId "Char#" () $ ModuleName "Builtin"
  string_t <- newExternalId "String#" () $ ModuleName "Builtin"
  ptr_t <- newExternalId "Ptr#" () $ ModuleName "Builtin"

  pure $
    RnEnv
      { _resolvedVarIdentMap = mempty,
        _resolvedTypeIdentMap =
          HashMap.fromList
            [ ("Int32#", [Qualified Implicit int32_t]),
              ("Int64#", [Qualified Implicit int64_t]),
              ("Float#", [Qualified Implicit float_t]),
              ("Double#", [Qualified Implicit double_t]),
              ("Char#", [Qualified Implicit char_t]),
              ("String#", [Qualified Implicit string_t]),
              ("Ptr#", [Qualified Implicit ptr_t])
            ],
        moduleName = modName,
        _uniqSupply = malgoEnv ^. uniqSupply,
        _srcName = malgoEnv ^. toLLOpt . srcName,
        _debugMode = malgoEnv ^. toLLOpt . debugMode,
        _modulePaths = malgoEnv ^. toLLOpt . modulePaths,
        _interfaces = malgoEnv ^. interfaces
      }

-- | Resolving a new (local) name
resolveName :: (MonadReader env m, MonadIO m, HasUniqSupply env UniqSupply) => Text -> m RnId
resolveName name = newInternalId name ()

-- | Resolving a new global (toplevel) name
resolveGlobalName :: (MonadReader env m, MonadIO m, HasUniqSupply env UniqSupply) => ModuleName -> Text -> m RnId
resolveGlobalName modName name = newExternalId name () modName

-- | Resolving a variable name that is already resolved
lookupVarName :: (MonadReader RnEnv m, MonadIO m) => Range -> Text -> m RnId
lookupVarName pos name =
  view (resolvedVarIdentMap . at name) >>= \case
    Just names -> case find (\(Qualified visi _) -> visi == Implicit) names of
      Just (Qualified _ name) -> pure name
      Nothing ->
        errorOn pos $
          "Not in scope:"
            <+> quotes (pPrint name)
            $$ "Did you mean"
            <+> pPrint names
    _ -> errorOn pos $ "Not in scope:" <+> quotes (pPrint name)

-- | Resolving a type name that is already resolved
lookupTypeName :: (MonadReader RnEnv m, MonadIO m) => Range -> Text -> m RnId
lookupTypeName pos name =
  view (resolvedTypeIdentMap . at name) >>= \case
    Just names -> case find (\(Qualified visi _) -> visi == Implicit) names of
      Just (Qualified _ name) -> pure name
      Nothing ->
        errorOn pos $
          "Not in scope:"
            <+> quotes (pPrint name)
            $$ "Did you mean"
            <+> pPrint names
    _ -> errorOn pos $ "Not in scope:" <+> quotes (pPrint name)

-- | Resolving a qualified variable name like Foo.x
lookupQualifiedVarName :: (MonadReader RnEnv m, MonadIO m) => Range -> ModuleName -> Text -> m (Id ())
lookupQualifiedVarName pos modName name =
  view (resolvedVarIdentMap . at name) >>= \case
    Just names ->
      case find (\(Qualified visi _) -> visi == Explicit modName) names of
        Just (Qualified _ name) -> pure name
        Nothing ->
          errorOn pos $
            "Not in scope:"
              <+> quotes (pPrint name)
              <+> "in"
              <+> pPrint modName
              $$ "Did you mean"
              <+> pPrint names
    _ -> errorOn pos $ "Not in scope:" <+> quotes (pPrint name)