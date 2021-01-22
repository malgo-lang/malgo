{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Language.Griff.TypeCheck.TcEnv where

import qualified Data.Map as Map
import Data.Store
import Koriel.MonadUniq
import Koriel.Pretty
import Language.Griff.Prelude
import Language.Griff.Rename.RnEnv (RnEnv)
import Language.Griff.Syntax.Extension
import Language.Griff.Type

data TcEnv = TcEnv
  { _varEnv :: Map RnId Scheme,
    _typeEnv :: Map RnTId TypeDef,
    _rnEnv :: RnEnv
  }
  deriving stock (Show, Eq)

instance Semigroup TcEnv where
  TcEnv v1 t1 r1 <> TcEnv v2 t2 r2 = TcEnv (v1 <> v2) (t1 <> t2) (r1 <> r2)

instance Monoid TcEnv where
  mempty = TcEnv mempty mempty mempty

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

data TypeDef = TypeDef {_constructor :: Type, _qualVars :: [TyVar], _union :: [(RnId, Type)]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Store)

instance Pretty TypeDef where
  pPrint (TypeDef c q u) = pPrint (c, q, u)

makeLenses ''TcEnv

makeLenses ''TypeDef

simpleTypeDef :: Type -> TypeDef
simpleTypeDef x = TypeDef x [] []

overTypeDef :: Monad f => (Type -> f Type) -> TypeDef -> f TypeDef
overTypeDef f = traverseOf constructor f <=< traverseOf (union . traversed . _2) f

genTcEnv :: MonadUniq m => RnEnv -> m TcEnv
genTcEnv rnEnv =
  pure $
    TcEnv
      { _varEnv = mempty,
        _typeEnv = mempty,
        _rnEnv = rnEnv
      }
