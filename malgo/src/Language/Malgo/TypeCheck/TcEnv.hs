{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.TypeCheck.TcEnv
  ( TcEnv (..),
    varEnv,
    typeEnv,
    rnEnv,
    TypeDef (..),
    typeConstructor,
    typeParameters,
    valueConstructors,
    genTcEnv,
  )
where

import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromJust)
import Koriel.Id
import Koriel.Pretty
import Language.Malgo.Prelude
import Language.Malgo.Rename.RnEnv (RnEnv)
import qualified Language.Malgo.Rename.RnEnv as R
import Language.Malgo.Syntax.Extension
import Language.Malgo.TypeRep.Static (IsType (fromType, safeToType), IsTypeDef (safeToTypeDef))
import qualified Language.Malgo.TypeRep.Static as Static
import Language.Malgo.TypeRep.UTerm
import Language.Malgo.UTerm

data TcEnv = TcEnv
  { _varEnv :: HashMap RnId Scheme,
    _typeEnv :: HashMap RnTId TypeDef,
    _rnEnv :: RnEnv
  }

instance Pretty TcEnv where
  pPrint TcEnv {_varEnv, _typeEnv, _rnEnv} =
    "TcEnv"
      <+> braces
        ( sep
            [ "_varEnv" <+> "=" <+> pPrint (HashMap.toList _varEnv),
              "_typeEnv" <+> "=" <+> pPrint (HashMap.toList _typeEnv),
              "_rnEnv" <+> "=" <+> pPrint _rnEnv
            ]
        )

instance HasUTerm TypeF TypeVar TcEnv where
  walkOn f TcEnv {_varEnv, _typeEnv, _rnEnv} =
    TcEnv <$> traverseOf (traversed . walkOn) f _varEnv
      <*> traverseOf (traversed . walkOn) f _typeEnv
      <*> pure _rnEnv

data TypeDef = TypeDef
  { _typeConstructor :: UType,
    _typeParameters :: [Id UType],
    _valueConstructors :: [(RnId, UType)]
  }

instance Pretty TypeDef where
  pPrint (TypeDef c q u) = pPrint (c, q, u)

instance HasUTerm TypeF TypeVar TypeDef where
  walkOn f TypeDef {_typeConstructor, _typeParameters, _valueConstructors} =
    TypeDef <$> f _typeConstructor
      <*> pure _typeParameters
      <*> traverseOf (traversed . _2 . walkOn) f _valueConstructors

instance IsTypeDef TypeDef where
  safeToTypeDef TypeDef {_typeConstructor, _typeParameters, _valueConstructors} =
    Static.TypeDef
      <$> safeToType _typeConstructor <*> traverse (idMeta safeToType) _typeParameters <*> traverse (_2 safeToType) _valueConstructors
  fromTypeDef Static.TypeDef {Static._typeConstructor, Static._typeParameters, Static._valueConstructors} =
    TypeDef (fromType _typeConstructor) (map (over idMeta fromType) _typeParameters) (map (over _2 fromType) _valueConstructors)

genTcEnv :: Applicative f => RnEnv -> f TcEnv
genTcEnv rnEnv = do
  let int32_t = fromJust $ find ((== ModuleName "Builtin") . view idMeta) =<< view (R.typeEnv . at "Int32#") rnEnv
  let int64_t = fromJust $ find ((== ModuleName "Builtin") . view idMeta) =<< view (R.typeEnv . at "Int64#") rnEnv
  let float_t = fromJust $ find ((== ModuleName "Builtin") . view idMeta) =<< view (R.typeEnv . at "Float#") rnEnv
  let double_t = fromJust $ find ((== ModuleName "Builtin") . view idMeta) =<< view (R.typeEnv . at "Double#") rnEnv
  let char_t = fromJust $ find ((== ModuleName "Builtin") . view idMeta) =<< view (R.typeEnv . at "Char#") rnEnv
  let string_t = fromJust $ find ((== ModuleName "Builtin") . view idMeta) =<< view (R.typeEnv . at "String#") rnEnv
  pure $
    TcEnv
      { _varEnv = mempty,
        _typeEnv =
          HashMap.fromList
            [ (int32_t, TypeDef (UTerm $ TyPrim Static.Int32T) [] []),
              (int64_t, TypeDef (UTerm $ TyPrim Static.Int64T) [] []),
              (float_t, TypeDef (UTerm $ TyPrim Static.FloatT) [] []),
              (double_t, TypeDef (UTerm $ TyPrim Static.DoubleT) [] []),
              (char_t, TypeDef (UTerm $ TyPrim Static.CharT) [] []),
              (string_t, TypeDef (UTerm $ TyPrim Static.StringT) [] [])
            ],
        _rnEnv = rnEnv
      }

makeLenses ''TcEnv
makeLenses ''TypeDef
