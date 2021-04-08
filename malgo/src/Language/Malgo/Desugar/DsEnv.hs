{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Desugar.DsEnv where

import qualified Data.HashMap.Strict as HashMap
import qualified Koriel.Core.Type as C
import Koriel.Id
import Koriel.Pretty
import Language.Malgo.Prelude
import Language.Malgo.Rename.RnEnv (RnEnv)
import Language.Malgo.Syntax.Extension
import Language.Malgo.TypeRep.Static

-- 脱糖衣処理の環境
data DsEnv = DsEnv
  { -- モジュール名
    _moduleName :: ModuleName,
    -- | Malgo -> Coreの名前環境
    _nameEnv :: HashMap RnId (Id C.Type),
    -- | 型環境
    _varTypeEnv :: HashMap RnId Scheme,
    _typeDefEnv :: HashMap RnTId (TypeDef Type),
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
              "_nameEnv" <+> "=" <+> pPrint (HashMap.toList _nameEnv),
              "_varTypeEnv" <+> "=" <+> pPrint (HashMap.toList _varTypeEnv),
              "_typeDefEnv" <+> "=" <+> pPrint (HashMap.toList _typeDefEnv),
              "_rnEnv" <+> "=" <+> pPrint _rnEnv
            ]
        )

makeLenses ''DsEnv

makeDsEnv ::
  IsScheme a =>
  ModuleName ->
  HashMap (Id ()) a ->
  HashMap (Id ()) (TypeDef Type) ->
  RnEnv ->
  DsEnv
makeDsEnv modName varEnv typeEnv rnEnv =
  DsEnv
    { _moduleName = modName,
      _nameEnv = mempty,
      _varTypeEnv = fmap toScheme varEnv,
      _typeDefEnv = typeEnv,
      _rnEnv = rnEnv
    }
