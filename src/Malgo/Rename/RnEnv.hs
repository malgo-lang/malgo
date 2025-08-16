{-# LANGUAGE UndecidableInstances #-}

-- | 'Malgo.Rename.RnEnv' contains functions which convert 'PsId' to 'RnId'.
module Malgo.Rename.RnEnv
  ( RenameError (..),
    Resolved,
    RnEnv (..),
    insertVarIdent,
    insertTypeIdent,
    addConstructors,
    isConstructor,
    genBuiltinRnEnv,
    resolveName,
    resolveGlobalName,
    lookupVarName,
    lookupTypeName,
    lookupQualifiedVarName,
  )
where

import Control.Exception (Exception)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Effectful (Eff, (:>))
import Effectful.Error.Static
import Effectful.Reader.Static (Reader, asks, runReader)
import Effectful.State.Static.Local (State)
import Malgo.Id
import Malgo.Module
import Malgo.Prelude
import Malgo.Syntax.Extension
import Prettyprinter (squotes, vsep, (<+>))

data RenameError
  = NoSuchNameInScope Range PsId [Resolved]
  | NotInScope Range PsId
  | NoSuchNameInModule Range PsId ModuleName [Resolved]
  | NotInModule Range PsId ModuleName
  | DuplicateName Range PsId
  | DuplicateNames Range [PsId]

instance Pretty RenameError where
  pretty (NoSuchNameInScope range name names) =
    vsep
      [ pretty range <> ":",
        "Not in scope:"
          <+> squotes (pretty name),
        "Did you mean"
          <+> pretty names
      ]
  pretty (NotInScope range name) =
    vsep
      [ pretty range <> ":",
        "Not in scope:"
          <+> squotes (pretty name)
      ]
  pretty (NoSuchNameInModule range name modName names) =
    vsep
      [ pretty range <> ":",
        "Not in scope:"
          <+> squotes (pretty name)
          <+> "in"
          <+> pretty modName,
        "Did you mean"
          <+> pretty names
      ]
  pretty (NotInModule range name modName) =
    vsep
      [ pretty range <> ":",
        "Not in scope:"
          <+> squotes (pretty name)
          <+> "in"
          <+> pretty modName
      ]
  pretty (DuplicateName range name) =
    vsep
      [ pretty range <> ":",
        "Duplicate name:"
          <+> squotes (pretty name)
      ]
  pretty (DuplicateNames range names) =
    vsep
      [ pretty range <> ":",
        "Duplicate names:"
          <+> pretty names
      ]

instance Show RenameError where
  show = show . pretty

instance Exception RenameError

-- | Resolved identifier
type Resolved = Qualified RnId

data RnEnv = RnEnv
  { -- | Environment for resolved variable identifiers.
    -- The key is the raw identifier (e.g. `foo`, `bar`).
    -- The value is the list of resolved identifiers (e.g. `foo`, `Foo.foo`, `B.bar`).
    resolvedVarIdentMap :: Map PsId [Resolved],
    resolvedTypeIdentMap :: Map PsId [Resolved],
    constructors :: Set Id,
    moduleNames :: Set ModuleName
  }

-- | 'insertVarIdent' resolved variable identifiers to the environment.
insertVarIdent :: [(PsId, Resolved)] -> RnEnv -> RnEnv
insertVarIdent newEnv RnEnv {..} = RnEnv {resolvedVarIdentMap = foldr (\(k, v) -> Map.insertWith (<>) k [v]) resolvedVarIdentMap newEnv, ..}

-- | 'insertTypeIdent' resolved type identifiers to the environment.
insertTypeIdent :: [(PsId, Resolved)] -> RnEnv -> RnEnv
insertTypeIdent newEnv RnEnv {..} = RnEnv {resolvedTypeIdentMap = foldr (\(k, v) -> Map.insertWith (<>) k [v]) resolvedTypeIdentMap newEnv, ..}

addConstructors :: [Id] -> RnEnv -> RnEnv
addConstructors cons env = env {constructors = Set.union (Set.fromList cons) env.constructors}

isConstructor :: Id -> RnEnv -> Bool
isConstructor con env = Set.member con env.constructors

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
      { resolvedVarIdentMap = mempty,
        resolvedTypeIdentMap =
          Map.fromList
            [ ("Int32#", [Qualified Implicit int32_t]),
              ("Int64#", [Qualified Implicit int64_t]),
              ("Float#", [Qualified Implicit float_t]),
              ("Double#", [Qualified Implicit double_t]),
              ("Char#", [Qualified Implicit char_t]),
              ("String#", [Qualified Implicit string_t]),
              ("Ptr#", [Qualified Implicit ptr_t])
            ],
        constructors = mempty,
        moduleNames = mempty
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
  asks @RnEnv ((.resolvedVarIdentMap) >>> Map.lookup name) >>= \case
    Just names -> case find (\(Qualified visi _) -> visi == Implicit) names of
      Just (Qualified _ name) -> pure name
      Nothing -> throwError $ NoSuchNameInScope pos name names
    _ -> throwError $ NotInScope pos name

-- | Resolving a type name that is already resolved
lookupTypeName :: (Reader RnEnv :> es, Error RenameError :> es) => Range -> Text -> Eff es Id
lookupTypeName pos name =
  asks @RnEnv ((.resolvedTypeIdentMap) >>> Map.lookup name) >>= \case
    Just names -> case find (\(Qualified visi _) -> visi == Implicit) names of
      Just (Qualified _ name) -> pure name
      Nothing -> throwError $ NoSuchNameInScope pos name names
    _ -> throwError $ NotInScope pos name

-- | Resolving a qualified variable name like Foo.x
lookupQualifiedVarName ::
  (Reader RnEnv :> es, Error RenameError :> es) =>
  Range ->
  ModuleName ->
  Text ->
  Eff es Id
lookupQualifiedVarName pos modName name =
  asks @RnEnv ((.resolvedVarIdentMap) >>> Map.lookup name) >>= \case
    Just names ->
      case find (\(Qualified visi _) -> visi == Explicit modName) names of
        Just (Qualified _ name) -> pure name
        Nothing -> throwError $ NoSuchNameInModule pos name modName names
    _ -> throwError $ NotInModule pos name modName
