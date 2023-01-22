{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Desugar.DsEnv (DsEnv (..), makeDsEnv) where

import Control.Lens.TH
import Koriel.Id
import Koriel.Lens
import Koriel.MonadUniq
import Malgo.Infer.TcEnv (TcEnv (TcEnv, _kindCtx))
import Malgo.Interface (Interface)
import Malgo.Monad
import Malgo.Prelude

-- 脱糖衣処理の環境
data DsEnv = DsEnv
  { _moduleName :: ModuleName,
    _uniqSupply :: UniqSupply,
    _modulePaths :: [FilePath],
    _interfaces :: IORef (HashMap ModuleName Interface)
  }

makeFieldsNoPrefix ''DsEnv

makeDsEnv :: ModuleName -> MalgoEnv -> TcEnv -> DsEnv
makeDsEnv _moduleName MalgoEnv {..} TcEnv {_kindCtx} = DsEnv {..}
