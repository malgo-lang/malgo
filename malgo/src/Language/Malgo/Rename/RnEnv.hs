{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Rename.RnEnv where

import qualified Data.HashMap.Strict as HashMap
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Pretty
import Language.Malgo.Prelude
import Language.Malgo.Syntax.Extension

data RnState = RnState {_infixInfo :: HashMap RnId (Assoc, Int), _moduleName :: ModuleName}
  deriving stock (Show)

instance Pretty RnState where
  pPrint RnState {_infixInfo, _moduleName} =
    "RnState"
      <+> braces
        ( sep
            [ "_infixInfo" <+> "=" <+> pPrint (HashMap.toList _infixInfo),
              "_moduleName" <+> "=" <+> pPrint _moduleName
            ]
        )

makeLenses ''RnState

data RnEnv = RnEnv
  { _varEnv :: HashMap PsId [RnId],
    _typeEnv :: HashMap PsTId [RnTId],
    _fieldEnv :: HashMap PsId [RnId],
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

makeLenses ''RnEnv

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

appendRnEnv :: ASetter' RnEnv (HashMap PsId [RnId]) -> [(PsId, RnId)] -> RnEnv -> RnEnv
appendRnEnv lens newEnv = over lens (go newEnv)
  where
    go [] e = e
    go ((n, n') : xs) e = go xs $ HashMap.alter (f n') n e
    f n' ns = Just $ (n' :) $ concat ns

genBuiltinRnEnv :: (MonadReader env m, HasUniqSupply env, MonadIO m) => MalgoEnv -> m RnEnv
genBuiltinRnEnv malgoEnv = do
  -- generate RnTId of primitive types
  int32_t <- newId "Int32#" () $ WiredIn $ ModuleName "Builtin"
  int64_t <- newId "Int64#" () $ WiredIn $ ModuleName "Builtin"
  float_t <- newId "Float#" () $ WiredIn $ ModuleName "Builtin"
  double_t <- newId "Double#" () $ WiredIn $ ModuleName "Builtin"
  char_t <- newId "Char#" () $ WiredIn $ ModuleName "Builtin"
  string_t <- newId "String#" () $ WiredIn $ ModuleName "Builtin"
  ptr_t <- newId "Ptr#" () $ WiredIn $ ModuleName "Builtin"

  pure $
    RnEnv
      { _varEnv = mempty,
        _typeEnv =
          HashMap.fromList
            [ ("Int32#", [int32_t]),
              ("Int64#", [int64_t]),
              ("Float#", [float_t]),
              ("Double#", [double_t]),
              ("Char#", [char_t]),
              ("String#", [string_t]),
              ("Ptr#", [ptr_t])
            ],
        _fieldEnv = HashMap.empty,
        _rnMalgoEnv = malgoEnv
      }
