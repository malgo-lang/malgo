-- | 'Malgo.Rename.RnEnv' contains functions which convert 'PsId' to 'RnId'.
module Malgo.Rename.RnEnv where

import Control.Lens (ASetter', At (at), Lens', lens, over, view, (^.))
import qualified Data.HashMap.Strict as HashMap
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Pretty
import Malgo.Prelude
import Malgo.Syntax.Extension
import Text.Megaparsec.Pos (SourcePos)

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
infixInfo = lens _infixInfo (\r x -> r {_infixInfo = x})

dependencies :: Lens' RnState [ModuleName]
dependencies = lens _dependencies (\r x -> r {_dependencies = x})

data Visibility
  = Explicit ModuleName -- variable that must be qualified
  | Implicit
  deriving stock (Show, Eq)

instance Pretty Visibility where pPrint = text . show

data RnEnv = RnEnv
  { _varEnv :: HashMap PsId [Annotated Visibility RnId],
    _typeEnv :: HashMap PsId [Annotated Visibility RnId],
    _fieldEnv :: HashMap PsId [Annotated Visibility RnId],
    _moduleName :: ModuleName,
    _rnMalgoEnv :: MalgoEnv
  }
  deriving stock (Show, Eq)

instance Pretty RnEnv where
  pPrint RnEnv {_varEnv, _typeEnv, _fieldEnv, _moduleName} =
    "RnEnv"
      <+> braces
        ( sep
            [ "_varEnv" <+> "=" <+> pPrint (HashMap.toList _varEnv),
              "_typeEnv" <+> "=" <+> pPrint (HashMap.toList _typeEnv),
              "_fieldEnv" <+> "=" <+> pPrint (HashMap.toList _fieldEnv),
              "_moduleName" <+> "=" <+> pPrint _moduleName
            ]
        )

varEnv :: Lens' RnEnv (HashMap PsId [Annotated Visibility RnId])
varEnv = lens _varEnv (\r x -> r {_varEnv = x})

typeEnv :: Lens' RnEnv (HashMap PsId [Annotated Visibility RnId])
typeEnv = lens _typeEnv (\r x -> r {_typeEnv = x})

fieldEnv :: Lens' RnEnv (HashMap PsId [Annotated Visibility RnId])
fieldEnv = lens _fieldEnv (\r x -> r {_fieldEnv = x})

moduleName :: Lens' RnEnv ModuleName
moduleName = lens _moduleName (\r x -> r {_moduleName = x})

rnMalgoEnv :: Lens' RnEnv MalgoEnv
rnMalgoEnv = lens _rnMalgoEnv (\r x -> r {_rnMalgoEnv = x})

class HasRnEnv env where
  rnEnv :: Lens' env RnEnv

instance HasRnEnv RnEnv where
  rnEnv = identity

instance HasMalgoEnv RnEnv where
  malgoEnv = rnMalgoEnv

instance HasOpt RnEnv where
  malgoOpt = rnMalgoEnv . malgoOpt

instance HasUniqSupply RnEnv where
  uniqSupply = rnMalgoEnv . uniqSupply

appendRnEnv :: ASetter' RnEnv (HashMap PsId [Annotated Visibility RnId]) -> [(PsId, Annotated Visibility RnId)] -> RnEnv -> RnEnv
appendRnEnv lens newEnv = over lens (go newEnv)
  where
    go [] e = e
    go ((n, n') : xs) e = go xs $ HashMap.alter (f n') n e
    f n' ns = Just $ (n' :) $ concat ns

genBuiltinRnEnv :: (MonadReader env m, HasUniqSupply env, MonadIO m) => ModuleName -> MalgoEnv -> m RnEnv
genBuiltinRnEnv modName malgoEnv = do
  -- generate RnId of primitive types
  int32_t <- newExternalId "Int32#" () $ ModuleName "Builtin"
  int64_t <- newExternalId "Int64#" () $ ModuleName "Builtin"
  float_t <- newExternalId "Float#" () $ ModuleName "Builtin"
  double_t <- newExternalId "Double#" () $ ModuleName "Builtin"
  char_t <- newExternalId "Char#" () $ ModuleName "Builtin"
  string_t <- newExternalId "String#" () $ ModuleName "Builtin"
  ptr_t <- newExternalId "Ptr#" () $ ModuleName "Builtin"

  pure $
    RnEnv
      { _varEnv = mempty,
        _typeEnv =
          HashMap.fromList
            [ ("Int32#", [Annotated Implicit int32_t]),
              ("Int64#", [Annotated Implicit int64_t]),
              ("Float#", [Annotated Implicit float_t]),
              ("Double#", [Annotated Implicit double_t]),
              ("Char#", [Annotated Implicit char_t]),
              ("String#", [Annotated Implicit string_t]),
              ("Ptr#", [Annotated Implicit ptr_t])
            ],
        _fieldEnv = HashMap.empty,
        _moduleName = modName,
        _rnMalgoEnv = malgoEnv
      }

-- | Resolving a new (local) name
resolveName :: (MonadReader env m, MonadIO m, HasUniqSupply env) => Text -> m RnId
resolveName name = newInternalId name ()

-- | Resolving a new global (toplevel) name
resolveGlobalName :: (MonadReader env m, MonadIO m, HasUniqSupply env) => ModuleName -> Text -> m RnId
resolveGlobalName modName name = newExternalId name () modName

-- | Resolving a variable name that is already resolved
lookupVarName :: (MonadReader RnEnv m, MonadIO m) => SourcePos -> Text -> m RnId
lookupVarName pos name =
  view (varEnv . at name) >>= \case
    Just names -> case find (\i -> i ^. ann == Implicit) names of
      Just (Annotated _ name) -> pure name
      Nothing ->
        errorOn pos $
          "Not in scope:" <+> quotes (pPrint name)
            $$ "Did you mean" <+> pPrint (map (view value) names)
    _ -> errorOn pos $ "Not in scope:" <+> quotes (pPrint name)

-- | Resolving a type name that is already resolved
lookupTypeName :: (MonadReader RnEnv m, MonadIO m) => SourcePos -> Text -> m RnId
lookupTypeName pos name =
  view (typeEnv . at name) >>= \case
    Just names -> case find (\i -> i ^. ann == Implicit) names of
      Just (Annotated _ name) -> pure name
      Nothing ->
        errorOn pos $
          "Not in scope:" <+> quotes (pPrint name)
            $$ "Did you mean" <+> pPrint (map (view value) names)
    _ -> errorOn pos $ "Not in scope:" <+> quotes (pPrint name)

-- | Resolving a field name that is already resolved
lookupFieldName :: (MonadReader RnEnv m, MonadIO m) => SourcePos -> Text -> m RnId
lookupFieldName pos name =
  view (fieldEnv . at name) >>= \case
    Just names -> case find (\i -> i ^. ann == Implicit) names of
      Just (Annotated _ name) -> pure name
      Nothing ->
        errorOn pos $
          "Not in scope:" <+> quotes (pPrint name)
            $$ "Did you mean" <+> pPrint (map (view value) names)
    _ -> errorOn pos $ "Not in scope:" <+> quotes (pPrint name)

-- | Resolving a qualified variable name like Foo.x
lookupQualifiedVarName :: (MonadReader RnEnv m, MonadIO m) => SourcePos -> ModuleName -> Text -> m (Id ())
lookupQualifiedVarName pos modName name =
  view (varEnv . at name) >>= \case
    Just names ->
      case find (\i -> i ^. ann == Explicit modName) names of
        Just (Annotated _ name) -> pure name
        Nothing ->
          errorOn pos $
            "Not in scope:" <+> quotes (pPrint name) <+> "in" <+> pPrint modName
              $$ "Did you mean" <+> "`" <> pPrint modName <+> "." <+> pPrint name <> "`" <+> "?"
    _ -> errorOn pos $ "Not in scope:" <+> quotes (pPrint name)

