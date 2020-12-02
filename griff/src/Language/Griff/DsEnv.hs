{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.DsEnv where

import qualified Koriel.Core.Type as C
import Koriel.Id
import Language.Griff.Extension
import Language.Griff.Prelude
import Language.Griff.TcEnv (TcEnv)

-- 脱糖衣処理の環境
data DsEnv = DsEnv
  { -- | Griff -> Coreの名前環境
    _varEnv :: Map TcId (Id C.Type),
    -- | 型環境
    _tcEnv :: TcEnv
  }
  deriving stock (Show)

instance Semigroup DsEnv where
  DsEnv v1 t1 <> DsEnv v2 t2 = DsEnv (v1 <> v2) (t1 <> t2)

instance Monoid DsEnv where
  mempty = DsEnv mempty mempty

varEnv :: Lens' DsEnv (Map TcId (Id C.Type))
varEnv = lens _varEnv (\e x -> e {_varEnv = x})

tcEnv :: Lens' DsEnv TcEnv
tcEnv = lens _tcEnv (\e x -> e {_tcEnv = x})
