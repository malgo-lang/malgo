{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | 'Malgo.Rename.RnEnv' contains functions which convert 'PsId' to 'RnId'.
module Malgo.Rename.RnEnv
  ( RenameError (..),
    Resolved,
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

import Control.Lens (ASetter', makeFieldsNoPrefix)
import Data.Map.Strict qualified as Map
import Effectful (Eff, (:>))
import Effectful.Error.Static
import Effectful.Reader.Static (Reader, asks, runReader)
import Effectful.State.Static.Local (State)
import Malgo.Id
import Malgo.Lens
import Malgo.Module
import Malgo.MonadUniq
import Malgo.Prelude hiding (throwError)
import Malgo.Syntax.Extension

data RenameError
  = NotInScope Range PsId [Resolved]
  | NotInModule Range PsId ModuleName [Resolved]

instance Pretty RenameError where
  pretty (NotInScope range name names) =
    vsep
      [ pretty range <> ":",
        "Not in scope:"
          <+> squotes (pretty name),
        "Did you mean"
          <+> pretty names
      ]
  pretty (NotInModule range name modName names) =
    vsep
      [ pretty range <> ":",
        "Not in scope:"
          <+> squotes (pretty name)
          <+> "in"
          <+> pretty modName,
        "Did you mean"
          <+> pretty names
      ]

instance Show RenameError where
  show = show . pretty

-- | Resolved identifier
type Resolved = Qualified RnId

data RnEnv = RnEnv
  { -- | Environment for resolved variable identifiers.
    -- The key is the raw identifier (e.g. `foo`, `bar`).
    -- The value is the list of resolved identifiers (e.g. `foo`, `Foo.foo`, `B.bar`).
    _resolvedVarIdentMap :: Map PsId [Resolved],
    _resolvedTypeIdentMap :: Map PsId [Resolved]
  }

makeFieldsNoPrefix ''RnEnv

-- | Append resolved identifiers to the environment.
appendRnEnv :: ASetter' RnEnv (Map PsId [Resolved]) -> [(PsId, Resolved)] -> RnEnv -> RnEnv
appendRnEnv lens newEnv = over lens
  $ \e -> foldr (\(k, v) -> Map.insertWith (<>) k [v]) e newEnv

-- | Generate RnId of primitive types
genBuiltinRnEnv :: Eff es RnEnv
genBuiltinRnEnv = runReader (ModuleName "Builtin") do
  int32_t <- newExternalId "Int32#"
  int64_t <- newExternalId "Int64#"
  float_t <- newExternalId "Float#"
  double_t <- newExternalId "Double#"
  char_t <- newExternalId "Char#"
  string_t <- newExternalId "String#"
  ptr_t <- newExternalId "Ptr#"

  pure
    $ RnEnv
      { _resolvedVarIdentMap = mempty,
        _resolvedTypeIdentMap =
          Map.fromList
            [ ("Int32#", [Qualified Implicit int32_t]),
              ("Int64#", [Qualified Implicit int64_t]),
              ("Float#", [Qualified Implicit float_t]),
              ("Double#", [Qualified Implicit double_t]),
              ("Char#", [Qualified Implicit char_t]),
              ("String#", [Qualified Implicit string_t]),
              ("Ptr#", [Qualified Implicit ptr_t])
            ]
      }

-- | Resolving a new (local) name
resolveName ::
  (State Uniq :> es, Reader ModuleName :> es) =>
  Text ->
  Eff es Id
resolveName = newInternalId

-- | Resolving a new global (toplevel) name
resolveGlobalName :: (Reader ModuleName :> es) => Text -> Eff es Id
resolveGlobalName = newExternalId

-- | Resolving a variable name that is already resolved
lookupVarName :: (Reader RnEnv :> es, Error RenameError :> es) => Range -> Text -> Eff es Id
lookupVarName pos name =
  asks @RnEnv ((._resolvedVarIdentMap) >>> Map.lookup name) >>= \case
    Just names -> case find (\(Qualified visi _) -> visi == Implicit) names of
      Just (Qualified _ name) -> pure name
      Nothing -> throwError $ NotInScope pos name names
    _ -> throwError $ NotInScope pos name []

-- | Resolving a type name that is already resolved
lookupTypeName :: (Reader RnEnv :> es, Error RenameError :> es) => Range -> Text -> Eff es Id
lookupTypeName pos name =
  asks @RnEnv ((._resolvedTypeIdentMap) >>> Map.lookup name) >>= \case
    Just names -> case find (\(Qualified visi _) -> visi == Implicit) names of
      Just (Qualified _ name) -> pure name
      Nothing -> throwError $ NotInScope pos name names
    _ -> throwError $ NotInScope pos name []

-- | Resolving a qualified variable name like Foo.x
lookupQualifiedVarName ::
  (Reader RnEnv :> es, Error RenameError :> es) =>
  Range ->
  ModuleName ->
  Text ->
  Eff es Id
lookupQualifiedVarName pos modName name =
  asks @RnEnv ((._resolvedVarIdentMap) >>> Map.lookup name) >>= \case
    Just names ->
      case find (\(Qualified visi _) -> visi == Explicit modName) names of
        Just (Qualified _ name) -> pure name
        Nothing -> throwError $ NotInModule pos name modName names
    _ -> throwError $ NotInModule pos name modName []
