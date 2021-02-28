{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Desugar.DsEnv where

import qualified Data.Map as Map
import qualified Koriel.Core.Type as C
import Koriel.Id
import Koriel.Pretty
import qualified Language.Malgo.NewTypeCheck.TcEnv as UTerm
import Language.Malgo.Prelude
import Language.Malgo.Rename.RnEnv (RnEnv)
import Language.Malgo.Syntax.Extension
import qualified Language.Malgo.TypeCheck.TcEnv as IORef
import Language.Malgo.TypeRep.Static

-- 脱糖衣処理の環境
data DsEnv = DsEnv
  { -- モジュール名
    _moduleName :: ModuleName,
    -- | Malgo -> Coreの名前環境
    _nameEnv :: Map TcId (Id C.Type),
    -- | 型環境
    _varTypeEnv :: Map TcId Scheme,
    _typeDefEnv :: Map TcId TypeDef,
    _rnEnv :: RnEnv
  }
  deriving stock (Show)

instance Semigroup DsEnv where
  DsEnv m1 n1 v1 t1 r1 <> DsEnv m2 n2 v2 t2 r2
    | m1 == ModuleName "$Undefined" = DsEnv m2 (n1 <> n2) (v1 <> v2) (t1 <> t2) (r1 <> r2)
    | m2 == ModuleName "$Undefined" = DsEnv m1 (n1 <> n2) (v1 <> v2) (t1 <> t2) (r1 <> r2)
    | m1 == m2 = DsEnv m1 (n1 <> n2) (v1 <> v2) (t1 <> t2) (r1 <> r2)
    | otherwise = errorDoc (pPrint m1 <+> "/=" <+> pPrint m2)

instance Monoid DsEnv where
  mempty = DsEnv (ModuleName "$Undefined") mempty mempty mempty mempty

instance Pretty DsEnv where
  pPrint DsEnv {_moduleName, _nameEnv, _varTypeEnv, _typeDefEnv, _rnEnv} =
    "DsEnv"
      <+> braces
        ( sep
            [ "_moduleName" <+> "=" <+> pPrint _moduleName,
              "_nameEnv" <+> "=" <+> pPrint (Map.toList _nameEnv),
              "_varTypeEnv" <+> "=" <+> pPrint (Map.toList _varTypeEnv),
              "_typeDefEnv" <+> "=" <+> pPrint (Map.toList _typeDefEnv),
              "_rnEnv" <+> "=" <+> pPrint _rnEnv
            ]
        )

makeLenses ''DsEnv

makeDsEnvFromIORef :: ModuleName -> IORef.TcEnv -> DsEnv
makeDsEnvFromIORef modName tcEnv =
  DsEnv
    { _moduleName = modName,
      _nameEnv = mempty,
      _varTypeEnv = fmap toScheme $ tcEnv ^. IORef.varEnv,
      _typeDefEnv = fmap toTypeDef $ tcEnv ^. IORef.typeEnv,
      _rnEnv = tcEnv ^. IORef.rnEnv
    }

makeDsEnvFromUTerm :: ModuleName -> UTerm.TcEnv -> DsEnv
makeDsEnvFromUTerm modName tcEnv =
  DsEnv
    { _moduleName = modName,
      _nameEnv = mempty,
      _varTypeEnv = fmap toScheme $ tcEnv ^. UTerm.varEnv,
      _typeDefEnv = fmap toTypeDef $ tcEnv ^. UTerm.typeEnv,
      _rnEnv = tcEnv ^. UTerm.rnEnv
    }

makeDsEnv :: (IsScheme a1, IsTypeDef a2) => ModuleName -> Map (Id ModuleName) a1 -> Map (Id ModuleName) a2 -> RnEnv -> DsEnv
makeDsEnv modName varEnv typeEnv rnEnv =
  DsEnv
    { _moduleName = modName,
      _nameEnv = mempty,
      _varTypeEnv = fmap toScheme varEnv,
      _typeDefEnv = fmap toTypeDef typeEnv,
      _rnEnv = rnEnv
    }
