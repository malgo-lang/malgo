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

module Language.Malgo.NewTypeCheck.TcEnv (TcEnv (..), varEnv, typeEnv, rnEnv, TypeDef (..), typeConstructor, typeParameters, valueConstructors) where

import qualified Data.Map as Map
import Koriel.Id
import Koriel.Pretty
import Language.Malgo.Prelude
import Language.Malgo.Rename.RnEnv (RnEnv)
import Language.Malgo.Syntax.Extension
import Language.Malgo.TypeRep.Static (IsKind (fromKind, safeToKind), IsType (fromType, safeToType), IsTypeDef (safeToTypeDef))
import qualified Language.Malgo.TypeRep.Static as Static
import Language.Malgo.TypeRep.UTerm
import Language.Malgo.Unify hiding (lookupVar)

data TcEnv = TcEnv
  { _varEnv :: Map RnId (Scheme UKind),
    _typeEnv :: Map RnTId TypeDef,
    _rnEnv :: RnEnv
  }

instance Pretty TcEnv where
  pPrint TcEnv {_varEnv, _typeEnv, _rnEnv} =
    "TcEnv"
      <+> braces
        ( sep
            [ "_varEnv" <+> "=" <+> pPrint (Map.toList _varEnv),
              "_typeEnv" <+> "=" <+> pPrint (Map.toList _typeEnv),
              "_rnEnv" <+> "=" <+> pPrint _rnEnv
            ]
        )

instance HasUTerm (TypeF UKind) (TypeVar UKind) TcEnv where
  walkOn f TcEnv {_varEnv, _typeEnv, _rnEnv} =
    TcEnv <$> traverseOf (traversed . walkOn) f _varEnv
      <*> traverseOf (traversed . walkOn) f _typeEnv
      <*> pure _rnEnv

data TypeDef = TypeDef
  { _typeConstructor :: UType,
    _typeParameters :: [Id UKind],
    _valueConstructors :: [(RnId, UType)]
  }

instance Pretty TypeDef where
  pPrint (TypeDef c q u) = pPrint (c, q, u)

instance HasUTerm (TypeF UKind) (TypeVar UKind) TypeDef where
  walkOn f TypeDef {_typeConstructor, _typeParameters, _valueConstructors} =
    TypeDef <$> f _typeConstructor
      <*> pure _typeParameters
      <*> traverseOf (traversed . _2 . walkOn) f _valueConstructors

instance IsTypeDef TypeDef where
  safeToTypeDef TypeDef {_typeConstructor, _typeParameters, _valueConstructors} =
    Static.TypeDef
      <$> safeToType _typeConstructor <*> traverse (idMeta safeToKind) _typeParameters <*> traverse (_2 safeToType) _valueConstructors
  fromTypeDef Static.TypeDef {Static._typeConstructor, Static._typeParameters, Static._valueConstructors} =
    TypeDef (fromType _typeConstructor) (map (over idMeta fromKind) _typeParameters) (map (over _2 fromType) _valueConstructors)

makeLenses ''TcEnv
makeLenses ''TypeDef
