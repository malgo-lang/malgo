{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Infer.TcEnv
  ( RecordTypeName,
    TcEnv (..),
    genTcEnv,
    findBuiltinType,
  )
where

import Control.Lens (At (at), makeFieldsNoPrefix, view, (^.))
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromJust)
import Koriel.Id
import Koriel.Lens
import Malgo.Infer.TypeRep
import {-# SOURCE #-} Malgo.Interface (Interface)
import Malgo.Prelude
import Malgo.Rename.RnEnv (Resolved, RnEnv)
import Malgo.Syntax.Extension

type RecordTypeName = Text

data TcEnv = TcEnv
  { _signatureMap :: HashMap RnId (Scheme Type),
    _typeDefMap :: HashMap RnId (TypeDef Type),
    _typeSynonymMap :: HashMap (Id Type) ([Id Type], Type),
    _resolvedTypeIdentMap :: HashMap PsId [Resolved],
    _interfaces :: IORef (HashMap ModuleName Interface)
  }

makeFieldsNoPrefix ''TcEnv

genTcEnv :: Applicative f => RnEnv -> f TcEnv
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
        _resolvedTypeIdentMap = rnEnv ^. resolvedTypeIdentMap,
        _interfaces = rnEnv ^. interfaces
      }

findBuiltinType :: (HasResolvedTypeIdentMap s (HashMap PsId [Resolved])) => PsId -> s -> Maybe RnId
findBuiltinType x rnEnv = do
  ids <- map (.value) <$> view (resolvedTypeIdentMap . at x) rnEnv
  find (isBuiltin) ids
  where
    isBuiltin :: RnId -> Bool
    isBuiltin Id {moduleName = ModuleName "Builtin", sort = External} = True
    isBuiltin _ = False