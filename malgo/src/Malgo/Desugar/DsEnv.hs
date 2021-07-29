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
    _fieldEnv :: HashMap RnId (Scheme Type),
    _desugarRnEnv :: RnEnv
  }
  deriving stock (Show)

instance Pretty DsEnv where
  pretty DsEnv {..} =
    "DsEnv"
      <+> braces
        ( sep
            [ "_moduleName" <+> "=" <+> pretty _moduleName,
              "_nameEnv" <+> "=" <+> pretty (HashMap.toList _nameEnv),
              "_varTypeEnv" <+> "=" <+> pretty (HashMap.toList _varTypeEnv),
              "_typeDefEnv" <+> "=" <+> pretty (HashMap.toList _typeDefEnv),
              "_fieldEnv" <+> "=" <+> pretty (HashMap.toList _fieldEnv),
              "_desugarRnEnv" <+> "=" <+> pretty _desugarRnEnv
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
  HashMap (Id ()) (Scheme Type) ->
  RnEnv ->
  DsEnv
makeDsEnv modName varEnv typeEnv fieldEnv rnEnv =
  DsEnv
    { _moduleName = modName,
      _nameEnv = mempty,
      _varTypeEnv = varEnv,
      _typeDefEnv = typeEnv,
      _fieldEnv = fieldEnv,
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
    Nothing -> errorDoc $ "Not in scope:" <+> squotes (pretty con)
