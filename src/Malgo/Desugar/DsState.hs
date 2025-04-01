{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Desugar.DsState
  ( Def (..),
    _VarDef,
    _FunDef,
    _ExtDef,
    DsState (..),
    globalClosures,
    HasNameEnv (..),
    HasGlobalDefs (..),
    makeDsState,
    lookupValueConstructors,
  )
where

import Control.Lens (mapped, traversed, (^.), _2)
import Control.Lens.TH
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Effectful (Eff, (:>))
import Effectful.State.Static.Local (State, gets)
import Malgo.Core.Syntax qualified as C
import Malgo.Core.Type qualified as C
import Malgo.Id
import Malgo.Infer.TypeRep
import Malgo.Infer.TypeRep qualified as GT
import Malgo.Lens
import Malgo.Prelude
import Malgo.Syntax.Extension
import Prettyprinter (squotes, (<+>))

-- | トップレベル宣言
data Def
  = VarDef (Meta C.Type) C.Type (C.Expr (Meta C.Type))
  | FunDef (Meta C.Type) [Meta C.Type] C.Type (C.Expr (Meta C.Type))
  | ExtDef Text C.Type
  deriving stock (Show)

makePrisms ''Def

-- | 'DsState' tracks the state of desugaring.
data DsState = DsState
  { -- | Name mapping from Malgo's 'RnId' to Core's 'Id'.
    nameEnv :: Map RnId (Meta C.Type),
    -- | Type signatures.
    signatureMap :: Map RnId (GT.Scheme GT.Type),
    -- | Type definitions.
    typeDefMap :: Map RnId (GT.TypeDef GT.Type),
    -- | Kind context.
    kindCtx :: KindCtx,
    -- | Top-level definitions.
    globalDefs :: [Def],
    -- | Closure Ids for global functions.
    globalClosures :: Map (Meta C.Type) (Meta C.Type)
  }
  deriving stock (Show)

makeFieldsId ''DsState

-- | 'makeDsStore' only takes 'TcEnv', but importing 'RnEnv' causes cyclic dependency.
makeDsState ::
  ( HasSignatureMap env (Map RnId (Scheme Type)),
    HasTypeDefMap env (Map Id (TypeDef Type)),
    HasKindCtx env KindCtx
  ) =>
  env ->
  DsState
makeDsState tcEnv =
  DsState
    { nameEnv = mempty,
      signatureMap = tcEnv ^. signatureMap,
      typeDefMap = tcEnv ^. typeDefMap,
      kindCtx = tcEnv ^. kindCtx,
      globalDefs = [],
      globalClosures = mempty
    }

lookupValueConstructors ::
  (State DsState :> es) =>
  GT.TypeVar ->
  [GT.Type] ->
  Eff es [(RnId, Scheme GT.Type)]
lookupValueConstructors con ts = do
  typeEnv <- gets @DsState (.typeDefMap)
  -- _valueConstructorsがnullのとき、そのフィールドは型シノニムのものなので無視する
  case List.find (\TypeDef {..} -> typeConstructor == GT.TyCon con && not (List.null valueConstructors)) (Map.elems typeEnv) of
    Just TypeDef {..} ->
      pure $ over (mapped . _2 . traversed) (GT.applySubst $ Map.fromList $ zip typeParameters ts) valueConstructors
    Nothing -> errorDoc $ "Not in scope:" <+> squotes (pretty con)
