{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | 'Malgo.Rename.RnEnv' contains functions which convert 'PsId' to 'RnId'.
module Malgo.Rename.RnEnv
  ( Resolved,
    RnEnv (..),
    appendRnEnv,
    genBuiltinRnEnv,
    resolveName,
    resolveGlobalName,
    lookupVarName,
    lookupTypeName,
    lookupQualifiedVarName,
  )
where

import Control.Lens (ASetter', At (at), makeFieldsNoPrefix, over, view, (^.))
import Data.HashMap.Strict qualified as HashMap
import Koriel.Id
import Koriel.Lens
import Koriel.MonadUniq
import Koriel.Pretty
import Malgo.Interface
import Malgo.Monad
import Malgo.Prelude
import Malgo.Syntax.Extension

-- | Resolved identifier
type Resolved = Qualified RnId

data RnEnv = RnEnv
  { -- | Environment for resolved variable identifiers.
    -- The key is the raw identifier (e.g. `foo`, `bar`).
    -- The value is the list of resolved identifiers (e.g. `foo`, `Foo.foo`, `B.bar`).
    _resolvedVarIdentMap :: HashMap PsId [Resolved],
    _resolvedTypeIdentMap :: HashMap PsId [Resolved],
    _moduleName :: ModuleName,
    _uniqSupply :: UniqSupply,
    debugMode :: Bool,
    _modulePaths :: [FilePath],
    _interfaces :: IORef (HashMap ModuleName Interface)
  }

makeFieldsNoPrefix ''RnEnv

-- | Append resolved identifiers to the environment.
appendRnEnv :: ASetter' RnEnv (HashMap PsId [Resolved]) -> [(PsId, Resolved)] -> RnEnv -> RnEnv
appendRnEnv lens newEnv = over lens $
  \e -> foldr (\(k, v) -> HashMap.insertWith (<>) k [v]) e newEnv

-- | Generate RnId of primitive types
genBuiltinRnEnv :: ModuleName -> MalgoM RnEnv
genBuiltinRnEnv modName = do
  malgoEnv <- ask
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
        _moduleName = modName,
        _uniqSupply = malgoEnv ^. uniqSupply,
        debugMode = malgoEnv.debugMode,
        _modulePaths = malgoEnv ^. modulePaths,
        _interfaces = malgoEnv ^. interfaces
      }

-- | Resolving a new (local) name
resolveName :: (MonadReader RnEnv m, MonadIO m) => Text -> m RnId
resolveName name = newInternalId name ()

-- | Resolving a new global (toplevel) name
resolveGlobalName :: (MonadReader RnEnv m, MonadIO m) => ModuleName -> Text -> m RnId
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
