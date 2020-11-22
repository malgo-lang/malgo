{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.RnEnv where

import qualified Data.Map as Map
import Koriel.Id
import Koriel.MonadUniq
import Language.Griff.Prelude
import Language.Griff.Extension

data RnState = RnState {_infixInfo :: Map RnId (Assoc, Int), _moduleName :: ModuleName}
  deriving stock (Show)

instance Semigroup RnState where
  RnState i1 p1 <> RnState i2 p2 | p1 == p2 = RnState (i1 <> i2) p1
    | otherwise = error "package name mismatch"

infixInfo :: Lens' RnState (Map RnId (Assoc, Int))
infixInfo = lens _infixInfo (\s x -> s { _infixInfo = x})

moduleName :: Getter RnState ModuleName
moduleName = to _moduleName

data RnEnv = RnEnv
  { _varEnv :: Map PsId RnId,
    _typeEnv :: Map PsTId RnTId
  }
  deriving stock (Show, Eq)

instance Semigroup RnEnv where
  RnEnv v1 t1 <> RnEnv v2 t2 = RnEnv (v1 <> v2) (t1 <> t2)

instance Monoid RnEnv where
  mempty = RnEnv mempty mempty

varEnv :: Lens' RnEnv (Map PsId RnId)
varEnv = lens _varEnv (\e x -> e { _varEnv = x})

typeEnv :: Lens' RnEnv (Map PsTId RnTId)
typeEnv = lens _typeEnv (\e x -> e { _typeEnv = x})

genRnState :: Monad m => ModuleName -> m RnState
genRnState (ModuleName name) = pure $ RnState mempty $ ModuleName name

genRnEnv :: MonadUniq m => m RnEnv
genRnEnv = do
  -- generate RnId of primitive functions and operetors
  add_i32 <- newId "add_i32#" $ ModuleName ""
  add_i64 <- newId "add_i64#" $ ModuleName ""
  -- generate RnTId of primitive types
  int32_t <- newId "Int32#" $ ModuleName ""
  int64_t <- newId "Int64#" $ ModuleName ""
  float_t <- newId "Float#" $ ModuleName ""
  double_t <- newId "Double#" $ ModuleName ""
  char_t <- newId "Char#" $ ModuleName ""
  string_t <- newId "String#" $ ModuleName ""
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
