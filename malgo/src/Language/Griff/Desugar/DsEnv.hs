{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Desugar.DsEnv where

import qualified Data.Map as Map
import qualified Koriel.Core.Type as C
import Koriel.Id
import Koriel.Pretty
import Language.Griff.Prelude
import Language.Griff.Syntax.Extension
import Language.Griff.TypeCheck.TcEnv (TcEnv)

-- 脱糖衣処理の環境
data DsEnv = DsEnv
  { -- モジュール名
    _moduleName :: ModuleName,
    -- | Griff -> Coreの名前環境
    _varEnv :: Map TcId (Id C.Type),
    -- | 型環境
    _tcEnv :: TcEnv
  }
  deriving stock (Show)

instance Semigroup DsEnv where
  DsEnv m1 v1 t1 <> DsEnv m2 v2 t2
    | m1 == ModuleName "$Undefined" = DsEnv m2 (v1 <> v2) (t1 <> t2)
    | m2 == ModuleName "$Undefined" = DsEnv m1 (v1 <> v2) (t1 <> t2)
    | m1 == m2 = DsEnv m1 (v1 <> v2) (t1 <> t2)
    | otherwise = errorDoc (pPrint m1 <+> "/=" <+> pPrint m2)

instance Monoid DsEnv where
  mempty = DsEnv (ModuleName "$Undefined") mempty mempty

instance Pretty DsEnv where
  pPrint DsEnv {_moduleName, _varEnv, _tcEnv} =
    "DsEnv"
      <+> braces
        ( sep
            [ "_moduleName" <+> "=" <+> pPrint _moduleName,
              "_varEnv" <+> "=" <+> pPrint (Map.toList _varEnv),
              "_tcEnv" <+> "=" <+> pPrint _tcEnv
            ]
        )

makeLenses ''DsEnv