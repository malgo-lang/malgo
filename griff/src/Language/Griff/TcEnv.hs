{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Language.Griff.TcEnv where

import qualified Data.Map as Map
import Data.Store
import Koriel.MonadUniq
import Koriel.Pretty
import Language.Griff.Extension
import Language.Griff.Prelude
import Language.Griff.RnEnv
  ( RnEnv,
  )
import qualified Language.Griff.RnEnv as R
import Language.Griff.Type

data TcEnv = TcEnv
  { _varEnv :: Map RnId Scheme,
    _typeEnv :: Map RnTId TypeDef,
    -- _tyConEnv :: Map TyCon ([TyVar], [(RnId, Type)]),
    _rnEnv :: RnEnv
  }
  deriving stock (Show, Eq)

instance Semigroup TcEnv where
  TcEnv v1 t1 r1 <> TcEnv v2 t2 r2 = TcEnv (v1 <> v2) (t1 <> t2) (r1 <> r2)

instance Monoid TcEnv where
  mempty = TcEnv mempty mempty mempty

data TypeDef = TypeDef {_constructor :: Type, _qualVars :: [TyVar], _union :: [(RnId, Type)]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Store)

instance Pretty TypeDef where
  pPrint (TypeDef c q u) = pPrint (c, q, u)

varEnv :: Lens' TcEnv (Map RnId Scheme)
varEnv = lens _varEnv (\e x -> e {_varEnv = x})

typeEnv :: Lens' TcEnv (Map RnTId TypeDef)
typeEnv = lens _typeEnv (\e x -> e {_typeEnv = x})

-- tyConEnv :: Lens' TcEnv (Map TyCon ([TyVar], [(RnId, Type)]))
-- tyConEnv = lens _tyConEnv (\e x -> e {_tyConEnv = x})

rnEnv :: Lens' TcEnv RnEnv
rnEnv = lens _rnEnv (\e x -> e {_rnEnv = x})

constructor :: Lens' TypeDef Type
constructor = lens _constructor (\e x -> e {_constructor = x})

qualVars :: Lens' TypeDef [TyVar]
qualVars = lens _qualVars (\e x -> e {_qualVars = x})

union :: Lens' TypeDef [(RnId, Type)]
union = lens _union (\e x -> e {_union = x})

simpleTypeDef :: Type -> TypeDef
simpleTypeDef x = TypeDef x [] []

overTypeDef :: Monad f => (Type -> f Type) -> TypeDef -> f TypeDef
overTypeDef f = traverseOf constructor f <=< traverseOf (union . traversed . _2) f

genTcEnv :: MonadUniq m => RnEnv -> m TcEnv
genTcEnv rnEnv = do
  -- lookup RnId of primitive functions and operetors
  let add_i32 = fromJust $ Map.lookup "add_i32#" (view R.varEnv rnEnv)
  let add_i64 = fromJust $ Map.lookup "add_i64#" (view R.varEnv rnEnv)
  -- lookup RnTId of primitive types
  let int32_t = fromJust $ Map.lookup "Int32#" (view R.typeEnv rnEnv)
  let int64_t = fromJust $ Map.lookup "Int64#" (view R.typeEnv rnEnv)
  let float_t = fromJust $ Map.lookup "Float#" (view R.typeEnv rnEnv)
  let double_t = fromJust $ Map.lookup "Double#" (view R.typeEnv rnEnv)
  let string_t = fromJust $ Map.lookup "String#" (view R.typeEnv rnEnv)
  pure $
    TcEnv
      { _varEnv =
          Map.fromList
            [ ( add_i32,
                Forall [] (TyTuple [TyPrim Int32T, TyPrim Int32T] `TyArr` TyPrim Int32T)
              ),
              ( add_i64,
                Forall [] (TyTuple [TyPrim Int64T, TyPrim Int64T] `TyArr` TyPrim Int64T)
              )
            ],
        _typeEnv =
          Map.fromList
            [ (int32_t, TypeDef (TyPrim Int32T) [] []),
              (int64_t, TypeDef (TyPrim Int64T) [] []),
              (float_t, TypeDef (TyPrim FloatT) [] []),
              (double_t, TypeDef (TyPrim DoubleT) [] []),
              (string_t, TypeDef (TyPrim StringT) [] [])
            ],
        _rnEnv = rnEnv
      }
