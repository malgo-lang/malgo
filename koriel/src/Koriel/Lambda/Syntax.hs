{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Koriel.Lambda.Syntax where

import Data.Int (Int32, Int64)
import Data.Kind (Constraint)
import Koriel.Id (Id)
import Koriel.Prelude

data Stmt x
  = DefVar (XDefVar x) (XId x) (Exp x)
  | DefType (XDefType x) (XId x) (Type x)

data Exp x
  = -- | 即値
    Atom (XAtom x) (Atom x)
  | -- | 変数
    Var (XVar x) (XId x)
  | -- | ラムダ抽象
    Lam (XLam x) (XId x) (Type x) (Exp x)
  | -- | 関数適用
    App (XApp x) (Exp x) (Exp x)
  | -- | 型抽象
    TLam (XTLam x) (XId x) (Kind x) (Exp x)
  | -- | 型適用
    TApp (XTApp x) (Exp x) (Type x)
  | -- | タグ付きの値
    Tag (XTag x) (XId x) (Exp x) (Type x)
  | -- | 場合分け [(タグ, 束縛変数, 式)]
    Case (XCase x) (Exp x) [(XId x, XId x, Exp x)]
  | -- | レコード
    Record (XRecord x) [(XId x, Exp x)]
  | -- | フィールドアクセス
    Proj (XProj x) (Exp x) (XId x)

data Atom x
  = Int32 Int32
  | Int64 Int64
  | Float Float
  | Double Double
  | Char Char
  | String String
  deriving stock (Eq, Ord, Show, Generic)

data Type x
  = -- | 組み込み型
    TyAtom (XTyAtom x) (TyAtom x)
  | -- | 型変数
    TyVar (XTyVar x) (XId x)
  | -- | 関数の型
    TyArr (XTyArr x) (Type x) (Type x)
  | -- | 全称型
    TyAll (XTyAll x) (XId x) (Kind x) (Type x)
  | -- | 型演算子抽象
    TyAbs (XTyAbs x) (XId x) (Kind x) (Type x)
  | -- | 型演算子適用
    TyApp (XTyApp x) (Type x) (Type x)
  | -- レコード型
    TyRecord (XTyRecord x) [(XId x, Type x)]
  | -- ヴァリアント型
    TyVariant (XTyVariant x) [(XId x, Type x)]

data TyAtom x = TyInt32 | TyInt64 | TyFloat | TyDouble | TyChar | TyString
  deriving stock (Eq, Ord, Show, Generic)

data Kind x
  = KStar (XKStar x) (KStar x)
  | KArr (XKArr x) (Kind x) (Kind x)

data KStar x = KBox | KInt32 | KInt64 | KFloat | KDouble | KChar | KString
  deriving stock (Eq, Ord, Show, Generic)

---------------
-- Instances --
---------------

-- Eq
deriving stock instance (Eq (XId x), ForallStmtX Eq x, ForallExpX Eq x, ForallTypeX Eq x, ForallKindX Eq x) => Eq (Stmt x)

deriving stock instance (Eq (XId x), ForallExpX Eq x, ForallTypeX Eq x, ForallKindX Eq x) => Eq (Exp x)

deriving stock instance (Eq (XId x), ForallTypeX Eq x, ForallKindX Eq x) => Eq (Type x)

deriving stock instance (ForallKindX Eq x) => Eq (Kind x)

-- Ord
deriving stock instance (Ord (XId x), ForallStmtX Ord x, ForallExpX Ord x, ForallTypeX Ord x, ForallKindX Ord x) => Ord (Stmt x)

deriving stock instance (Ord (XId x), ForallExpX Ord x, ForallTypeX Ord x, ForallKindX Ord x) => Ord (Exp x)

deriving stock instance (Ord (XId x), ForallTypeX Ord x, ForallKindX Ord x) => Ord (Type x)

deriving stock instance (ForallKindX Ord x) => Ord (Kind x)

-- Show
deriving stock instance (Show (XId x), ForallStmtX Show x, ForallExpX Show x, ForallTypeX Show x, ForallKindX Show x) => Show (Stmt x)

deriving stock instance (Show (XId x), ForallExpX Show x, ForallTypeX Show x, ForallKindX Show x) => Show (Exp x)

deriving stock instance (Show (XId x), ForallTypeX Show x, ForallKindX Show x) => Show (Type x)

deriving stock instance (ForallKindX Show x) => Show (Kind x)

-- Generic
deriving stock instance (Generic (XId x), ForallStmtX Generic x, ForallExpX Generic x, ForallTypeX Generic x, ForallKindX Generic x) => Generic (Stmt x)

deriving stock instance (Generic (XId x), ForallExpX Generic x, ForallTypeX Generic x, ForallKindX Generic x) => Generic (Exp x)

deriving stock instance (Generic (XId x), ForallTypeX Generic x, ForallKindX Generic x) => Generic (Type x)

deriving stock instance (ForallKindX Generic x) => Generic (Kind x)

----------------
-- Extensions --
----------------

data Koriel (p :: Phase)

data Phase = Raw | Rename | TypeCheck

type family XId x = r | r -> x

type instance XId (Koriel 'Raw) = String

type instance XId (Koriel 'Rename) = Id ()

type instance XId (Koriel 'TypeCheck) = Id (Type (Koriel 'TypeCheck))

type family SimpleX x where
  SimpleX (Koriel 'Raw) = ()
  SimpleX (Koriel 'Rename) = ()
  SimpleX (Koriel 'TypeCheck) = ()

type family XDefVar x where
  XDefVar x = SimpleX x

type family XDefType x where
  XDefType x = SimpleX x

type ForallStmtX c x = (c (XDefVar x), c (XDefType x)) :: Constraint

type family XAtom x where
  XAtom x = SimpleX x

type family XVar x where
  XVar x = SimpleX x

type family XLam x where
  XLam x = SimpleX x

type family XApp x where
  XApp x = SimpleX x

type family XTLam x where
  XTLam x = SimpleX x

type family XTApp x where
  XTApp x = SimpleX x

type family XTag x where
  XTag x = SimpleX x

type family XCase x where
  XCase x = SimpleX x

type family XRecord x where
  XRecord x = SimpleX x

type family XProj x where
  XProj x = SimpleX x

type ForallExpX c x = (c (XAtom x), c (XVar x), c (XLam x), c (XApp x), c (XTLam x), c (XTApp x), c (XTag x), c (XCase x), c (XRecord x), c (XProj x)) :: Constraint

type family XTyAtom x where
  XTyAtom x = SimpleX x

type family XTyVar x where
  XTyVar x = SimpleX x

type family XTyApp x where
  XTyApp x = SimpleX x

type family XTyArr x where
  XTyArr x = SimpleX x

type family XTyAll x where
  XTyAll x = SimpleX x

type family XTyAbs x where
  XTyAbs x = SimpleX x

type family XTyRecord x where
  XTyRecord x = SimpleX x

type family XTyVariant x where
  XTyVariant x = SimpleX x

type ForallTypeX c x = (c (XTyAtom x), c (XTyVar x), c (XTyApp x), c (XTyArr x), c (XTyAll x), c (XTyAbs x), c (XTyRecord x), c (XTyVariant x)) :: Constraint

type family XKStar x where
  XKStar x = SimpleX x

type family XKArr x where
  XKArr x = SimpleX x

type ForallKindX c x = (c (XKStar x), c (XKArr x)) :: Constraint
