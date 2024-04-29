{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Desugar.DsState
  ( Def (..),
    _VarDef,
    _FunDef,
    _ExtDef,
    DsState (..),
    HasNameEnv (..),
    HasGlobalDefs (..),
    makeDsState,
    lookupValueConstructors,
  )
where

import Control.Lens (mapped, traversed, (^.), _2)
import Control.Lens.TH
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Effectful (Eff, (:>))
import Effectful.State.Static.Local (State, gets)
import Koriel.Core.Syntax qualified as C
import Koriel.Core.Type qualified as C
import Koriel.Id
import Koriel.Lens
import Malgo.Infer.TypeRep
import Malgo.Infer.TypeRep qualified as GT
import Malgo.Prelude
import Malgo.Syntax.Extension

-- | トップレベル宣言
data Def
  = VarDef (Meta C.Type) C.Type (C.Expr (Meta C.Type))
  | FunDef (Meta C.Type) [Meta C.Type] C.Type (C.Expr (Meta C.Type))
  | ExtDef Text C.Type
  deriving stock (Show)

makePrisms ''Def

-- | 'DsState' tracks the state of desugaring.
data DsState = DsState
  { -- | Name mapping from Malgo's 'RnId' to Koriel's 'Id'.
    _nameEnv :: HashMap RnId (Meta C.Type),
    -- | Type signatures.
    _signatureMap :: HashMap RnId (GT.Scheme GT.Type),
    -- | Type definitions.
    _typeDefMap :: HashMap RnId (GT.TypeDef GT.Type),
    -- | Kind context.
    _kindCtx :: KindCtx,
    -- | Top-level definitions.
    _globalDefs :: [Def],
    -- | Closure Ids for global functions.
    globalClosures :: HashMap (Meta C.Type) (Meta C.Type)
  }
  deriving stock (Show)

makeFieldsNoPrefix ''DsState

-- | 'makeDsStore' only takes 'TcEnv', but importing 'RnEnv' causes cyclic dependency.
makeDsState ::
  (HasSignatureMap env (HashMap RnId (Scheme Type)), HasTypeDefMap env (HashMap Id (TypeDef Type)), HasKindCtx env KindCtx) =>
  env ->
  DsState
makeDsState tcEnv =
  DsState
    { _nameEnv = mempty,
      _signatureMap = tcEnv ^. signatureMap,
      _typeDefMap = tcEnv ^. typeDefMap,
      _kindCtx = tcEnv ^. kindCtx,
      _globalDefs = [],
      globalClosures = mempty
    }

lookupValueConstructors ::
  (State DsState :> es) =>
  GT.TypeVar ->
  [GT.Type] ->
  Eff es [(RnId, Scheme GT.Type)]
lookupValueConstructors con ts = do
  typeEnv <- gets @DsState (._typeDefMap)
  -- _valueConstructorsがnullのとき、そのフィールドは型シノニムのものなので無視する
  case List.find (\TypeDef {..} -> _typeConstructor == GT.TyCon con && not (List.null _valueConstructors)) (HashMap.elems typeEnv) of
    Just TypeDef {..} ->
      pure $ over (mapped . _2 . traversed) (GT.applySubst $ HashMap.fromList $ zip _typeParameters ts) _valueConstructors
    Nothing -> errorDoc $ "Not in scope:" <+> squotes (pretty con)
