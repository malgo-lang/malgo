{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Infer.TcEnv
  ( RecordTypeName,
    TcEnv (..),
    genTcEnv,
    insertSignature,
    insertTypeDef,
    updateTypeDef,
    insertTypeSynonym,
    insertKind,
    mergeInterface,
  )
where

import Control.Lens (At (at), makeFieldsId, view, (%~), (^.))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Malgo.Id
import Malgo.Infer.TypeRep hiding (insertKind)
import Malgo.Infer.TypeRep qualified as TypeRep
import Malgo.Interface (Interface (..), externalFromInterface)
import Malgo.Lens
import Malgo.Module
import Malgo.Prelude
import Malgo.Rename.RnEnv (Resolved)
import Malgo.Syntax.Extension

type RecordTypeName = Text

data TcEnv = TcEnv
  { signatureMap :: Map RnId (Scheme Type),
    typeDefMap :: Map RnId (TypeDef Type),
    typeSynonymMap :: Map TypeVar ([TypeVar], Type),
    resolvedTypeIdentMap :: Map PsId [Resolved],
    kindCtx :: KindCtx
  }
  deriving stock (Show)

makeFieldsId ''TcEnv

insertSignature :: RnId -> Scheme Type -> TcEnv -> TcEnv
insertSignature name scheme = over signatureMap (Map.insert name scheme)

insertTypeDef :: RnId -> TypeDef Type -> TcEnv -> TcEnv
insertTypeDef name def = over typeDefMap (Map.insert name def)

updateTypeDef :: RnId -> (TypeDef Type -> TypeDef Type) -> TcEnv -> TcEnv
updateTypeDef name f = over typeDefMap (Map.adjust f name)

insertTypeSynonym :: TypeVar -> ([TypeVar], Type) -> TcEnv -> TcEnv
insertTypeSynonym name def = over typeSynonymMap (Map.insert name def)

insertKind :: RnId -> Kind -> TcEnv -> TcEnv
insertKind name kind = over kindCtx (TypeRep.insertKind name kind)

mergeInterface :: Interface -> TcEnv -> TcEnv
mergeInterface interface tcEnv =
  tcEnv
    & ( signatureMap
          %~ Map.union
            ( Map.mapKeys
                (externalFromInterface interface)
                interface.signatureMap
            )
      )
    & ( typeDefMap
          %~ Map.union
            (Map.mapKeys (externalFromInterface interface) interface.typeDefMap)
      )
    & (typeSynonymMap %~ Map.union interface.typeSynonymMap)
    & (kindCtx %~ Map.union interface.kindCtx)

genTcEnv :: (Applicative f, HasResolvedTypeIdentMap rnEnv (Map Text [Qualified Id])) => rnEnv -> f TcEnv
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
      { signatureMap = mempty,
        typeDefMap =
          Map.fromList
            [ (int32_t, TypeDef (TyPrim Int32T) [] []),
              (int64_t, TypeDef (TyPrim Int64T) [] []),
              (float_t, TypeDef (TyPrim FloatT) [] []),
              (double_t, TypeDef (TyPrim DoubleT) [] []),
              (char_t, TypeDef (TyPrim CharT) [] []),
              (string_t, TypeDef (TyPrim StringT) [] []),
              (ptr_t, TypeDef TyPtr [] [])
            ],
        typeSynonymMap = mempty,
        resolvedTypeIdentMap = rnEnv ^. resolvedTypeIdentMap,
        kindCtx =
          Map.fromList
            [ (int32_t, TYPE),
              (int64_t, TYPE),
              (float_t, TYPE),
              (double_t, TYPE),
              (char_t, TYPE),
              (string_t, TYPE),
              (ptr_t, TYPE `TyArr` TYPE)
            ]
      }

findBuiltinType ::
  (HasResolvedTypeIdentMap rnEnv (Map Text [Qualified Id])) =>
  PsId -> rnEnv -> Maybe RnId
findBuiltinType x rnEnv = do
  ids <- map (.value) <$> view (resolvedTypeIdentMap . at x) rnEnv
  find isBuiltin ids
  where
    isBuiltin :: RnId -> Bool
    isBuiltin Id {moduleName = ModuleName "Builtin", sort = External} = True
    isBuiltin _ = False
