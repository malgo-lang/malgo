{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Infer.TcEnv
  ( RecordTypeName,
    TcEnv (..),
    genTcEnv,
  )
where

import Control.Lens (At (at), makeFieldsNoPrefix, to, view)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromJust)
import GHC.Records (HasField)
import Koriel.Id
import Koriel.Lens
import Malgo.Infer.TypeRep
import Malgo.Interface (Interface)
import Malgo.Prelude
import Malgo.Syntax.Extension

type RecordTypeName = Text

data TcEnv = TcEnv
  { _signatureMap :: HashMap RnId (Scheme Type),
    _typeDefMap :: HashMap RnId (TypeDef Type),
    _typeSynonymMap :: HashMap TypeVar ([TypeVar], Type),
    _resolvedTypeIdentMap :: HashMap PsId [Qualified RnId],
    _kindCtx :: KindCtx,
    _interfaces :: IORef (HashMap ModuleName Interface)
  }

makeFieldsNoPrefix ''TcEnv

genTcEnv ::
  ( Applicative f,
    HasField "_resolvedTypeIdentMap" r (HashMap Text [Qualified RnId]),
    HasField "_interfaces" r (IORef (HashMap ModuleName Interface))
  ) =>
  r ->
  f TcEnv
genTcEnv rnEnv = do
  let int32_t = fromJust $ findBuiltinType "Int32#" rnEnv
  let int64_t = fromJust $ findBuiltinType "Int64#" rnEnv
  let float_t = fromJust $ findBuiltinType "Float#" rnEnv
  let double_t = fromJust $ findBuiltinType "Double#" rnEnv
  let char_t = fromJust $ findBuiltinType "Char#" rnEnv
  let string_t = fromJust $ findBuiltinType "String#" rnEnv
  let ptr_t = fromJust $ findBuiltinType "Ptr#" rnEnv
  pure $
    TcEnv
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
        _resolvedTypeIdentMap = rnEnv._resolvedTypeIdentMap,
        _kindCtx =
          HashMap.fromList
            [ (int32_t, TYPE),
              (int64_t, TYPE),
              (float_t, TYPE),
              (double_t, TYPE),
              (char_t, TYPE),
              (string_t, TYPE),
              (ptr_t, TYPE `TyArr` TYPE)
            ],
        _interfaces = rnEnv._interfaces
      }

findBuiltinType :: (HasField "_resolvedTypeIdentMap" r (HashMap Text [Qualified RnId])) => PsId -> r -> Maybe RnId
findBuiltinType x rnEnv = do
  ids <- map (.value) <$> view (to (._resolvedTypeIdentMap) . at x) rnEnv
  find isBuiltin ids
  where
    isBuiltin :: RnId -> Bool
    isBuiltin Id {moduleName = ModuleName "Builtin", sort = External} = True
    isBuiltin _ = False