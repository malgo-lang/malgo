{-# LANGUAGE DerivingVia #-}

module Malgo.Infer.TcEnv
  ( TcEnv (..),
    initTcEnv,
    insertSignature,
    insertTypeDef,
    updateTypeDef,
    insertTypeSynonym,
    mergeInterface,
  )
where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Effectful (Eff, (:>))
import Effectful.State.Static.Local (State, modify)
import GHC.Generics (Generically (..))
import GHC.Records (HasField)
import Malgo.Id
import Malgo.Infer.Kind (KindCtx)
import Malgo.Infer.TypeRep
import Malgo.Interface (Interface (..), externalFromInterface)
import Malgo.Module
import Malgo.Prelude
import Malgo.Rename.RnEnv (Resolved)
import Malgo.Syntax.Extension

data TcEnv = TcEnv
  { signatureMap :: Map RnId (Scheme Type),
    typeDefMap :: Map RnId (TypeDef Type),
    typeSynonymMap :: Map TypeVar ([TypeVar], Type),
    resolvedTypeIdentMap :: Map PsId [Resolved]
  }
  deriving stock (Show, Generic)
  deriving (Semigroup, Monoid) via Generically TcEnv

instance Pretty TcEnv where
  -- TcEnv is a large structure, so we just print its type
  pretty TcEnv {} = "TcEnv"

insertSignature :: RnId -> Scheme Type -> TcEnv -> TcEnv
insertSignature name scheme TcEnv {..} =
  TcEnv {signatureMap = Map.insert name scheme signatureMap, ..}

insertTypeDef :: RnId -> TypeDef Type -> TcEnv -> TcEnv
insertTypeDef name def TcEnv {..} =
  TcEnv {typeDefMap = Map.insert name def typeDefMap, ..}

updateTypeDef :: RnId -> (TypeDef Type -> TypeDef Type) -> TcEnv -> TcEnv
updateTypeDef name f TcEnv {..} =
  TcEnv {typeDefMap = Map.adjust f name typeDefMap, ..}

insertTypeSynonym :: TypeVar -> ([TypeVar], Type) -> TcEnv -> TcEnv
insertTypeSynonym name def TcEnv {..} =
  TcEnv {typeSynonymMap = Map.insert name def typeSynonymMap, ..}

mergeInterface :: (State TcEnv :> es, State KindCtx :> es) => Interface -> Eff es ()
mergeInterface interface = do
  modify @TcEnv \TcEnv {..} ->
    TcEnv
      { signatureMap = Map.union (Map.mapKeys (externalFromInterface interface) interface.signatureMap) signatureMap,
        typeDefMap = Map.union (Map.mapKeys (externalFromInterface interface) interface.typeDefMap) typeDefMap,
        typeSynonymMap = Map.union typeSynonymMap interface.typeSynonymMap,
        ..
      }
  modify @KindCtx (Map.union interface.kindCtx)

initTcEnv :: (State TcEnv :> es, HasField "resolvedTypeIdentMap" rnEnv (Map Text [Qualified Id]), State KindCtx :> es) => rnEnv -> Eff es ()
initTcEnv rnEnv = do
  let int32_t = fromJust $ findBuiltinType "Int32#" rnEnv
  let int64_t = fromJust $ findBuiltinType "Int64#" rnEnv
  let float_t = fromJust $ findBuiltinType "Float#" rnEnv
  let double_t = fromJust $ findBuiltinType "Double#" rnEnv
  let char_t = fromJust $ findBuiltinType "Char#" rnEnv
  let string_t = fromJust $ findBuiltinType "String#" rnEnv
  let ptr_t = fromJust $ findBuiltinType "Ptr#" rnEnv
  modify \tcEnv ->
    tcEnv
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
        resolvedTypeIdentMap = rnEnv.resolvedTypeIdentMap
      }
  modify @KindCtx \_ ->
    Map.fromList
      [ (int32_t, TYPE),
        (int64_t, TYPE),
        (float_t, TYPE),
        (double_t, TYPE),
        (char_t, TYPE),
        (string_t, TYPE),
        (ptr_t, TYPE `TyArr` TYPE)
      ]

findBuiltinType ::
  (HasField "resolvedTypeIdentMap" rnEnv (Map Text [Qualified Id])) =>
  PsId -> rnEnv -> Maybe RnId
findBuiltinType x rnEnv = do
  ids <- map (.value) <$> Map.lookup x rnEnv.resolvedTypeIdentMap
  find isBuiltin ids
  where
    isBuiltin :: RnId -> Bool
    isBuiltin Id {moduleName = ModuleName "Builtin", sort = External} = True
    isBuiltin _ = False
