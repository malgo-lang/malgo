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
    _typeEnv :: HashMap PsTId [RnTId]
  }
  deriving stock (Show, Eq)

instance Semigroup RnEnv where
  RnEnv v1 t1 <> RnEnv v2 t2 = RnEnv (append v1 v2) (append t1 t2)
    where
      append v1 v2 = HashMap.foldrWithKey (\k ns -> HashMap.alter (f ns) k) v2 v1
      f ns1 Nothing = Just ns1
      f ns1 (Just ns2) = Just (ns1 <> ns2)

instance Monoid RnEnv where
  mempty = RnEnv mempty mempty

instance Pretty RnEnv where
  pPrint RnEnv {_varEnv, _typeEnv} =
    "RnEnv"
      <+> braces
        ( sep
            [ "_varEnv" <+> "=" <+> pPrint (HashMap.toList _varEnv),
              "_typeEnv" <+> "=" <+> pPrint (HashMap.toList _typeEnv)
            ]
        )

makeLenses ''RnEnv

appendRnEnv :: ASetter' RnEnv (HashMap PsId [RnId]) -> [(PsId, RnId)] -> RnEnv -> RnEnv
appendRnEnv lens newEnv = over lens (go newEnv)
  where
    go [] e = e
    go ((n, n') : xs) e = go xs $ HashMap.alter (f n') n e
    f n' Nothing = Just [n']
    f n' (Just ns) = Just (n' : ns)

genBuiltinRnEnv :: MonadUniq m => m RnEnv
genBuiltinRnEnv = do
  -- generate RnTId of primitive types
  int32_t <- newTopLevelId "Int32#" $ ModuleName "Builtin"
  int64_t <- newTopLevelId "Int64#" $ ModuleName "Builtin"
  float_t <- newTopLevelId "Float#" $ ModuleName "Builtin"
  double_t <- newTopLevelId "Double#" $ ModuleName "Builtin"
  char_t <- newTopLevelId "Char#" $ ModuleName "Builtin"
  string_t <- newTopLevelId "String#" $ ModuleName "Builtin"
  ptr_t <- newTopLevelId "Ptr#" $ ModuleName "Builtin"

  -- generate RnId of primitive functions
  add_i32 <- newTopLevelId "add_Int32#" $ ModuleName "Builtin"
  add_i64 <- newTopLevelId "add_Int64#" $ ModuleName "Builtin"

  pure $
    RnEnv
      { _varEnv =
          HashMap.fromList
            [ ("add_Int32#", [add_i32]),
              ("add_Int64#", [add_i64])
            ],
        _typeEnv =
          HashMap.fromList
            [ ("Int32#", [int32_t]),
              ("Int64#", [int64_t]),
              ("Float#", [float_t]),
              ("Double#", [double_t]),
              ("Char#", [char_t]),
              ("String#", [string_t]),
              ("Ptr#", [ptr_t])
            ]
      }
