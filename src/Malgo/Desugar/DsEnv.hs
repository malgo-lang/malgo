{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Desugar.DsEnv where

import Control.Lens (mapped, over, traversed, use, (^.), _2)
import Control.Lens.TH
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Koriel.Core.Type qualified as C
import Koriel.Id
import Koriel.Lens
import Koriel.MonadUniq
import Koriel.Pretty
import Malgo.Infer.TcEnv (TcEnv)
import Malgo.Infer.TypeRep
import Malgo.Infer.TypeRep qualified as GT
import {-# SOURCE #-} Malgo.Interface (Interface)
import Malgo.Prelude
import Malgo.Syntax.Extension

-- 脱糖衣処理の環境
data DsEnv = DsEnv
  { _moduleName :: ModuleName,
    _uniqSupply :: UniqSupply,
    _modulePaths :: [FilePath],
    _interfaces :: IORef (HashMap ModuleName Interface)
  }

makeFieldsNoPrefix ''DsEnv

makeDsEnv :: ModuleName -> MalgoEnv -> DsEnv
makeDsEnv _moduleName MalgoEnv {..} =
  let _modulePaths = _toLLOpt._modulePaths
   in DsEnv {..}

data DsState = DsState
  { -- | Malgo -> Coreの名前環境
    _nameEnv :: HashMap RnId (Id C.Type),
    -- | 型環境
    _signatureMap :: HashMap RnId (GT.Scheme GT.Type),
    _typeDefMap :: HashMap RnId (GT.TypeDef GT.Type)
  }

makeFieldsNoPrefix ''DsState

makeDsState ::
  TcEnv ->
  DsState
makeDsState tcEnv =
  DsState
    { _nameEnv = mempty,
      _signatureMap = tcEnv ^. signatureMap,
      _typeDefMap = tcEnv ^. typeDefMap
    }

lookupValueConstructors ::
  MonadState DsState m =>
  Id GT.Type ->
  [GT.Type] ->
  m [(RnId, Scheme GT.Type)]
lookupValueConstructors con ts = do
  typeEnv <- use typeDefMap
  -- _valueConstructorsがnullのとき、そのフィールドは型シノニムのものなので無視する
  case List.find (\TypeDef {..} -> _typeConstructor == GT.TyCon con && not (List.null _valueConstructors)) (HashMap.elems typeEnv) of
    Just TypeDef {..} ->
      pure $ over (mapped . _2 . traversed) (GT.applySubst $ HashMap.fromList $ zip _typeParameters ts) _valueConstructors
    Nothing -> errorDoc $ "Not in scope:" <+> quotes (pPrint con)
