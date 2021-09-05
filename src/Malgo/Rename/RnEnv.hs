module Malgo.Rename.RnEnv where

import qualified Data.HashMap.Strict as HashMap
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Pretty
import Malgo.Prelude
import Malgo.Syntax.Extension

data RnState = RnState
  { _infixInfo :: HashMap RnId (Assoc, Int),
    _dependencies :: [ModuleName],
    _moduleName :: ModuleName
  }
  deriving stock (Show)

instance Pretty RnState where
  pPrint RnState {_infixInfo, _dependencies, _moduleName} =
    "RnState"
      <+> braces
        ( sep
            [ sep ["_infixInfo", "=", pPrint $ HashMap.toList _infixInfo],
              sep ["_dependencies", "=", pPrint _dependencies],
              sep ["_moduleName", "=", pPrint _moduleName]
            ]
        )

infixInfo :: Lens' RnState (HashMap RnId (Assoc, Int))
infixInfo = lens _infixInfo (\r x -> r {_infixInfo = x})

dependencies :: Lens' RnState [ModuleName]
dependencies = lens _dependencies (\r x -> r {_dependencies = x})

moduleName :: Lens' RnState ModuleName
moduleName = lens _moduleName (\r x -> r {_moduleName = x})

data Visibility
  = Explicit ModuleName -- must be qualified
  | Implicit
  deriving stock (Show, Eq)

instance Pretty Visibility where pPrint = text . show

data RnEnv = RnEnv
  { _varEnv :: HashMap PsId [With Visibility RnId],
    _typeEnv :: HashMap PsId [With Visibility RnId],
    _fieldEnv :: HashMap PsId [With Visibility RnId],
    _rnMalgoEnv :: MalgoEnv
  }
  deriving stock (Show, Eq)

instance Pretty RnEnv where
  pPrint RnEnv {_varEnv, _typeEnv, _fieldEnv} =
    "RnEnv"
      <+> braces
        ( sep
            [ "_varEnv" <+> "=" <+> pPrint (HashMap.toList _varEnv),
              "_typeEnv" <+> "=" <+> pPrint (HashMap.toList _typeEnv),
              "_fieldEnv" <+> "=" <+> pPrint (HashMap.toList _fieldEnv)
            ]
        )

varEnv :: Lens' RnEnv (HashMap PsId [With Visibility RnId])
varEnv = lens _varEnv (\r x -> r {_varEnv = x})

typeEnv :: Lens' RnEnv (HashMap PsId [With Visibility RnId])
typeEnv = lens _typeEnv (\r x -> r {_typeEnv = x})

fieldEnv :: Lens' RnEnv (HashMap PsId [With Visibility RnId])
fieldEnv = lens _fieldEnv (\r x -> r {_fieldEnv = x})

rnMalgoEnv :: Lens' RnEnv MalgoEnv
rnMalgoEnv = lens _rnMalgoEnv (\r x -> r {_rnMalgoEnv = x})

class HasRnEnv env where
  rnEnv :: Lens' env RnEnv

instance HasRnEnv RnEnv where
  rnEnv = lens id const

instance HasMalgoEnv RnEnv where
  malgoEnv = rnMalgoEnv

instance HasOpt RnEnv where
  malgoOpt = rnMalgoEnv . malgoOpt

instance HasUniqSupply RnEnv where
  uniqSupply = rnMalgoEnv . uniqSupply

instance HasLogFunc RnEnv where
  logFuncL = rnMalgoEnv . logFuncL

appendRnEnv :: ASetter' RnEnv (HashMap PsId [With Visibility RnId]) -> [(PsId, With Visibility RnId)] -> RnEnv -> RnEnv
appendRnEnv lens newEnv = over lens (go newEnv)
  where
    go [] e = e
    go ((n, n') : xs) e = go xs $ HashMap.alter (f n') n e
    f n' ns = Just $ (n' :) $ concat ns

genBuiltinRnEnv :: (MonadReader env m, HasUniqSupply env, MonadIO m) => MalgoEnv -> m RnEnv
genBuiltinRnEnv malgoEnv = do
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
            [ ("Int32#", [With Implicit int32_t]),
              ("Int64#", [With Implicit int64_t]),
              ("Float#", [With Implicit float_t]),
              ("Double#", [With Implicit double_t]),
              ("Char#", [With Implicit char_t]),
              ("String#", [With Implicit string_t]),
              ("Ptr#", [With Implicit ptr_t])
            ],
        _fieldEnv = HashMap.empty,
        _rnMalgoEnv = malgoEnv
      }
