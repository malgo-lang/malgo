{-# LANGUAGE TemplateHaskell #-}

module Malgo.Desugar.DsEnv where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Koriel.Core.Type as C
import Koriel.Id
import Koriel.Pretty
import Malgo.Prelude
import Malgo.Rename.RnEnv (HasRnEnv (rnEnv), RnEnv)
import Malgo.Syntax.Extension
import Malgo.TypeRep.Static
import qualified Malgo.TypeRep.Static as GT

-- 脱糖衣処理の環境
data DsEnv = DsEnv
  { -- モジュール名
    _moduleName :: ModuleName,
    -- | Malgo -> Coreの名前環境
    _nameEnv :: HashMap RnId (Id C.Type),
    -- | 型環境
    _varTypeEnv :: HashMap RnId (Scheme Type),
    _typeDefEnv :: HashMap RnId (TypeDef Type),
    _desugarRnEnv :: RnEnv
  }
  deriving stock (Show)

instance Pretty DsEnv where
  pPrint DsEnv {..} =
    "DsEnv"
      <+> braces
        ( sep
            [ "_moduleName" <+> "=" <+> pPrint _moduleName,
              "_nameEnv" <+> "=" <+> pPrint (HashMap.toList _nameEnv),
              "_varTypeEnv" <+> "=" <+> pPrint (HashMap.toList _varTypeEnv),
              "_typeDefEnv" <+> "=" <+> pPrint (HashMap.toList _typeDefEnv),
              "_desugarRnEnv" <+> "=" <+> pPrint _desugarRnEnv
            ]
        )

makeLenses ''DsEnv

class HasDsEnv env where
  dsEnv :: Lens' env DsEnv

instance HasDsEnv DsEnv where
  dsEnv = lens id const

instance HasRnEnv DsEnv where
  rnEnv = desugarRnEnv

makeDsEnv ::
  ModuleName ->
  HashMap (Id ()) (Scheme Type) ->
  HashMap (Id ()) (TypeDef Type) ->
  RnEnv ->
  DsEnv
makeDsEnv modName varEnv typeEnv rnEnv =
  DsEnv
    { _moduleName = modName,
      _nameEnv = mempty,
      _varTypeEnv = varEnv,
      _typeDefEnv = typeEnv,
      _desugarRnEnv = rnEnv
    }

lookupValueConstructors ::
  MonadState DsEnv m =>
  Id GT.Type ->
  [GT.Type] ->
  m [(RnId, Scheme GT.Type)]
lookupValueConstructors con ts = do
  typeEnv <- use typeDefEnv
  -- _valueConstructorsがnullのとき、そのフィールドは型シノニムのものなので無視する
  case List.find (\TypeDef {..} -> _typeConstructor == GT.TyCon con && not (List.null _valueConstructors)) (HashMap.elems typeEnv) of
    Just TypeDef {..} ->
      pure $ over (mapped . _2 . traversed) (GT.applySubst $ HashMap.fromList $ zip _typeParameters ts) _valueConstructors
    Nothing -> errorDoc $ "Not in scope:" <+> quotes (pPrint con)
