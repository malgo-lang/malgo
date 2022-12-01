{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Desugar.DsState where

import Control.Lens (mapped, over, traversed, use, (^.), _2)
import Control.Lens.TH
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Koriel.Core.Syntax qualified as C
import Koriel.Core.Type qualified as C
import Koriel.Id
import Koriel.Lens
import Koriel.Pretty
import Malgo.Infer.TypeRep
import Malgo.Infer.TypeRep qualified as GT
import Malgo.Prelude
import Malgo.Syntax.Extension

-- | トップレベル宣言
data Def
  = VarDef (Id C.Type) (C.Exp (Id C.Type))
  | FunDef (Id C.Type) ([Id C.Type], C.Exp (Id C.Type))
  | ExtDef Text C.Type

makePrisms ''Def

data DsState = DsState
  { -- | Malgo -> Coreの名前環境
    _nameEnv :: HashMap RnId (Id C.Type),
    -- | 型環境
    _signatureMap :: HashMap RnId (GT.Scheme GT.Type),
    _typeDefMap :: HashMap RnId (GT.TypeDef GT.Type),
    _globalDefs :: [Def]
  }

makeFieldsNoPrefix ''DsState

-- | 'makeDsStore' only takes 'TcEnv', but importing 'RnEnv' causes cyclic dependency.
makeDsState ::
  (HasSignatureMap env (HashMap RnId (Scheme Type)), HasTypeDefMap env (HashMap (Id ()) (TypeDef Type))) =>
  env ->
  DsState
makeDsState tcEnv =
  DsState
    { _nameEnv = mempty,
      _signatureMap = tcEnv ^. signatureMap,
      _typeDefMap = tcEnv ^. typeDefMap,
      _globalDefs = []
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