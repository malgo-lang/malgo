{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.DesugarEnv where

import qualified Koriel.Core.Type as C
import Koriel.Id
import Language.Griff.Extension
import Language.Griff.Prelude
import Language.Griff.TcEnv (TcEnv)

-- 脱糖衣処理の環境
data DesugarEnv = DesugarEnv
  { -- | Griff -> Coreの名前環境
    _varEnv :: Map TcId (Id C.Type),
    -- | 型環境
    _tcEnv :: TcEnv
  }
  deriving stock (Show)

instance Semigroup DesugarEnv where
  DesugarEnv v1 t1 <> DesugarEnv v2 t2 = DesugarEnv (v1 <> v2) (t1 <> t2)

instance Monoid DesugarEnv where
  mempty = DesugarEnv mempty mempty

varEnv :: Lens' DesugarEnv (Map TcId (Id C.Type))
varEnv = lens _varEnv (\e x -> e {_varEnv = x})

tcEnv :: Lens' DesugarEnv TcEnv
tcEnv = lens _tcEnv (\e x -> e {_tcEnv = x})
