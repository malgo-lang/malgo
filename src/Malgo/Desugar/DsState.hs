{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Desugar.DsState
  ( Def (..),
    _VarDef,
    _FunDef,
    _ExtDef,
    DsState (..),
    makeDsState,
    lookupValueConstructors,
  )
where

import Control.Lens (mapped, over, traversed, (^.), _2)
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
    nameEnv :: HashMap RnId (Id C.Type),
    -- | 変数-型
    signatureMap :: HashMap RnId (GT.Scheme GT.Type),
    -- | 型名-型定義
    typeDefMap :: HashMap RnId (GT.TypeDef GT.Type),
    -- | 型変数-カインド
    kindCtx :: KindCtx,
    -- | トップレベル宣言
    globalDefs :: [Def]
  }

makeFieldsNoPrefix ''DsState

-- | 'makeDsStore' only takes 'TcEnv', but importing 'RnEnv' causes cyclic dependency.
makeDsState ::
  (HasSignatureMap env (HashMap RnId (Scheme Type)), HasKindCtx env KindCtx, HasTypeDefMap env (HashMap (Id ()) (TypeDef Type))) =>
  env ->
  DsState
makeDsState tcEnv =
  DsState
    { nameEnv = mempty,
      signatureMap = tcEnv ^. signatureMap,
      typeDefMap = tcEnv ^. typeDefMap,
      kindCtx = tcEnv ^. kindCtx,
      globalDefs = []
    }

lookupValueConstructors ::
  MonadState DsState m =>
  -- | 型コンストラクタ
  GT.TypeVar ->
  -- | 型実引数
  [GT.Type] ->
  m [(RnId, Scheme GT.Type)]
lookupValueConstructors con ts = do
  typeEnv <- gets (.typeDefMap)
  -- valueConstructorsがnullのとき、そのフィールドは型シノニムのものなので無視する
  -- TODO: 型シノニムを考慮する必要があるのか？考慮する必要がないため無視するのなら、assertionでチェックすべきでは？
  case List.find (\TypeDef {..} -> typeConstructor == GT.TyCon con && not (List.null valueConstructors)) (HashMap.elems typeEnv) of
    Just TypeDef {..} ->
      pure $ over (mapped . _2 . traversed) (GT.applySubst $ HashMap.fromList $ zip typeParameters ts) valueConstructors
    Nothing -> errorDoc $ "Not in scope:" <+> quotes (pPrint con)