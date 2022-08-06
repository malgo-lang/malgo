{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Infer.TcEnv
  ( RecordTypeName,
    TcEnv (..),
    genTcEnv,
    findBuiltinType,
    appendFieldBelongMap,
  )
where

import Control.Lens (At (at), makeFieldsNoPrefix, over, view, (^.))
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromJust)
import Koriel.Id
import Koriel.Lens
import Koriel.Pretty
import Malgo.Infer.TypeRep
import Malgo.Prelude
import Malgo.Rename.RnEnv (RnEnv)
import qualified Malgo.Rename.RnEnv as R
import Malgo.Syntax.Extension
import Text.Pretty.Simple (pShow)

type RecordTypeName = Text

data TcEnv = TcEnv
  { _signatureMap :: HashMap RnId (Scheme Type),
    _typeDefMap :: HashMap RnId (TypeDef Type),
    _typeSynonymMap :: HashMap (Id Type) ([Id Type], Type),
    _fieldBelongMap :: HashMap [Text] [(RecordTypeName, Scheme Type)],
    _resolvedTypeIdentMap :: HashMap PsId [Annotated R.Visibility RnId]
  }
  deriving stock (Show)

makeFieldsNoPrefix ''TcEnv

instance Pretty TcEnv where
  pPrint env = Koriel.Pretty.text $ toString $ pShow env

appendFieldBelongMap :: [([Text], (RecordTypeName, Scheme Type))] -> TcEnv -> TcEnv
appendFieldBelongMap newEnv = over fieldBelongMap (go newEnv)
  where
    go [] e = e
    go ((n, n') : xs) e = go xs $ HashMap.alter (f n') n e
    f n' ns = Just $ (n' :) $ concat ns

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
        _fieldBelongMap = mempty,
        _resolvedTypeIdentMap = rnEnv ^. resolvedTypeIdentMap
      }

findBuiltinType :: (HasResolvedTypeIdentMap s (HashMap PsId [Annotated R.Visibility RnId])) => PsId -> s -> Maybe RnId
findBuiltinType x rnEnv = do
  ids <- map (view value) <$> view (resolvedTypeIdentMap . at x) rnEnv
  find (view idSort >>> \case External (ModuleName "Builtin") -> True; _ -> False) ids
