{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Desugar.DsEnv where

import Control.Lens (mapped, over, traversed, use, (^.), _2)
import Control.Lens.TH
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Koriel.Core.Type as C
import Koriel.Id
import Koriel.Lens
import Koriel.Pretty
import Malgo.Prelude
import Malgo.Syntax.Extension
import Malgo.TypeCheck.TcEnv (TcEnv)
import Malgo.TypeRep
import qualified Malgo.TypeRep as GT
import Text.Pretty.Simple (pShow)

-- 脱糖衣処理の環境
data DsEnv = DsEnv
  { -- モジュール名
    _moduleName :: ModuleName,
    -- | Malgo -> Coreの名前環境
    _nameEnv :: HashMap RnId (Id C.Type),
    -- | 型環境
    _signatureMap :: HashMap RnId (GT.Scheme GT.Type),
    _typeDefMap :: HashMap RnId (GT.TypeDef GT.Type)
  }
  deriving stock (Show)

instance Pretty DsEnv where
  pPrint dsEnv = Koriel.Pretty.text $ toString $ pShow dsEnv

makeFieldsNoPrefix ''DsEnv

makeDsEnv ::
  ModuleName ->
  TcEnv ->
  DsEnv
makeDsEnv modName tcEnv =
  DsEnv
    { _moduleName = modName,
      _nameEnv = mempty,
      _signatureMap = tcEnv ^. signatureMap,
      _typeDefMap = tcEnv ^. typeDefMap
    }

lookupValueConstructors ::
  MonadState DsEnv m =>
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
