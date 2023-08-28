{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Infer.TcEnv
  ( RecordTypeName,
    TcEnv,
    genTcEnv,
    insertSignature,
    insertTypeDef,
    updateTypeDef,
    insertTypeSynonym,
    insertKind,
    mergeInterface,
  )
where

import Control.Lens (At (at), makeFieldsNoPrefix, view, (%~), (^.))
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromJust)
import Koriel.Id
import Koriel.Lens
import Malgo.Infer.TypeRep hiding (insertKind)
import Malgo.Infer.TypeRep qualified as TypeRep
import Malgo.Interface (Interface)
import Malgo.Prelude
import Malgo.Rename.RnEnv (Resolved, RnEnv)
import Malgo.Syntax.Extension

type RecordTypeName = Text

data TcEnv = TcEnv
  { _signatureMap :: HashMap RnId (Scheme Type),
    _typeDefMap :: HashMap RnId (TypeDef Type),
    _typeSynonymMap :: HashMap TypeVar ([TypeVar], Type),
    _resolvedTypeIdentMap :: HashMap PsId [Resolved],
    _kindCtx :: KindCtx
  }

makeFieldsNoPrefix ''TcEnv

insertSignature :: RnId -> Scheme Type -> TcEnv -> TcEnv
insertSignature name scheme = over signatureMap (HashMap.insert name scheme)

insertTypeDef :: RnId -> TypeDef Type -> TcEnv -> TcEnv
insertTypeDef name def = over typeDefMap (HashMap.insert name def)

updateTypeDef :: RnId -> (TypeDef Type -> TypeDef Type) -> TcEnv -> TcEnv
updateTypeDef name f = over typeDefMap (HashMap.adjust f name)

insertTypeSynonym :: TypeVar -> ([TypeVar], Type) -> TcEnv -> TcEnv
insertTypeSynonym name def = over typeSynonymMap (HashMap.insert name def)

insertKind :: RnId -> Kind -> TcEnv -> TcEnv
insertKind name kind = over kindCtx (TypeRep.insertKind name kind)

mergeInterface :: Interface -> TcEnv -> TcEnv
mergeInterface interface tcEnv =
  tcEnv
    & (signatureMap %~ HashMap.union (interface ^. signatureMap))
    & (typeDefMap %~ HashMap.union (interface ^. typeDefMap))
    & (typeSynonymMap %~ HashMap.union (interface ^. typeSynonymMap))
    & (kindCtx %~ HashMap.union (interface ^. kindCtx))

genTcEnv :: (Applicative f) => RnEnv -> f TcEnv
genTcEnv rnEnv = do
  let int32_t = fromJust $ findBuiltinType "Int32#" rnEnv
  let int64_t = fromJust $ findBuiltinType "Int64#" rnEnv
  let float_t = fromJust $ findBuiltinType "Float#" rnEnv
  let double_t = fromJust $ findBuiltinType "Double#" rnEnv
  let char_t = fromJust $ findBuiltinType "Char#" rnEnv
  let string_t = fromJust $ findBuiltinType "String#" rnEnv
  let ptr_t = fromJust $ findBuiltinType "Ptr#" rnEnv
  pure
    $ TcEnv
      { _signatureMap = mempty,
        _typeDefMap =
          HashMap.fromList
            [ (int32_t, TypeDef (TyPrim Int32T) [] []),
              (int64_t, TypeDef (TyPrim Int64T) [] []),
              (float_t, TypeDef (TyPrim FloatT) [] []),
              (double_t, TypeDef (TyPrim DoubleT) [] []),
              (char_t, TypeDef (TyPrim CharT) [] []),
              (string_t, TypeDef (TyPrim StringT) [] []),
              (ptr_t, TypeDef TyPtr [] [])
            ],
        _typeSynonymMap = mempty,
        _resolvedTypeIdentMap = rnEnv ^. resolvedTypeIdentMap,
        _kindCtx =
          HashMap.fromList
            [ (int32_t, TYPE),
              (int64_t, TYPE),
              (float_t, TYPE),
              (double_t, TYPE),
              (char_t, TYPE),
              (string_t, TYPE),
              (ptr_t, TYPE `TyArr` TYPE)
            ]
      }

findBuiltinType :: PsId -> RnEnv -> Maybe RnId
findBuiltinType x rnEnv = do
  ids <- map (.value) <$> view (resolvedTypeIdentMap . at x) rnEnv
  find isBuiltin ids
  where
    isBuiltin :: RnId -> Bool
    isBuiltin Id {moduleName = ModuleName "Builtin", sort = External} = True
    isBuiltin _ = False
