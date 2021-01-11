{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Rename.RnEnv where

import qualified Data.Map as Map
import qualified Data.Text.Lazy as TL
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Pretty
import Language.Griff.Prelude
import Language.Griff.Syntax.Extension

data RnState = RnState {_infixInfo :: Map RnId (Assoc, Int), _moduleName :: ModuleName}
  deriving stock (Show)

instance Pretty RnState where pPrint = text . TL.unpack . pShow

makeLenses ''RnState

data RnEnv = RnEnv
  { _varEnv :: Map PsId RnId,
    _typeEnv :: Map PsTId RnTId
  }
  deriving stock (Show, Eq)

instance Semigroup RnEnv where
  RnEnv v1 t1 <> RnEnv v2 t2 = RnEnv (v1 <> v2) (t1 <> t2)

instance Monoid RnEnv where
  mempty = RnEnv mempty mempty

instance Pretty RnEnv where
  pPrint RnEnv {_varEnv, _typeEnv} =
    "RnEnv"
      <+> braces
        ( sep
            [ "_varEnv" <+> "=" <+> pPrint (Map.toList _varEnv),
              "_typeEnv" <+> "=" <+> pPrint (Map.toList _typeEnv)
            ]
        )

makeLenses ''RnEnv

genBuiltinRnEnv :: MonadUniq m => m RnEnv
genBuiltinRnEnv = do
  -- generate RnId of primitive functions and operetors
  add_i32 <- newTopLevelId "add_i32#" $ ModuleName "Builtin"
  add_i64 <- newTopLevelId "add_i64#" $ ModuleName "Builtin"

  -- generate RnTId of primitive types
  int32_t <- newTopLevelId "Int32#" $ ModuleName "Builtin"
  int64_t <- newTopLevelId "Int64#" $ ModuleName "Builtin"
  float_t <- newTopLevelId "Float#" $ ModuleName "Builtin"
  double_t <- newTopLevelId "Double#" $ ModuleName "Builtin"
  char_t <- newTopLevelId "Char#" $ ModuleName "Builtin"
  string_t <- newTopLevelId "String#" $ ModuleName "Builtin"
  pure $
    RnEnv
      { _varEnv = Map.fromList [("add_i32#", add_i32), ("add_i64#", add_i64)],
        _typeEnv =
          Map.fromList
            [ ("Int32#", int32_t),
              ("Int64#", int64_t),
              ("Float#", float_t),
              ("Double#", double_t),
              ("Char#", char_t),
              ("String#", string_t)
            ]
      }
