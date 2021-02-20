{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.KindF where

import Data.Binary hiding (get)
import Data.Deriving
import Data.Fix
import qualified Data.Map as Map
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Pretty
import Language.Malgo.Prelude
import Language.Malgo.Unify

----------------------
-- Kind and HasKind --
----------------------

-- | Runtime representation
data Rep
  = -- | Boxed value
    BoxedRep
  | -- | Int32#
    Int32Rep
  | -- | Int64#
    Int64Rep
  | -- | Float#
    FloatRep
  | -- | Double#
    DoubleRep
  | -- | Char#
    CharRep
  | -- | String#
    StringRep
  deriving stock (Eq, Ord, Show, Generic)

instance Binary Rep

instance Pretty Rep where pPrint rep = text $ show rep

-- | Definition of `kind`
data KindF a
  = -- | a kind
    Type Rep
  | -- | kind arrow (* -> *, * is a kind)
    KArr a a
  deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

instance Binary a => Binary (KindF a)

deriveEq1 ''KindF
deriveOrd1 ''KindF
deriveShow1 ''KindF

instance Pretty1 KindF where
  liftPPrintPrec _ _ _ (Type rep) = pPrint rep
  liftPPrintPrec ppr l d (KArr k1 k2) =
    maybeParens (d > 10) $ ppr l 11 k1 <+> "->" <+> ppr l 10 k2

instance Pretty a => Pretty (KindF a) where
  pPrintPrec l d k = liftPPrintPrec pPrintPrec l d k

newtype KindVar = KindVar (Id ())
  deriving newtype (Eq, Ord, Show, Pretty)

type UKind = UTerm KindF KindVar

type Kind = Fix KindF

instance Var KindF KindVar where
  isRigid _ = False
  rigidName = lens (const "") const
  toRigidName = const ""
  toBound _ _ = pure $ Fix $ Type BoxedRep

instance Unifiable KindF KindVar where
  unify x (Type rep1) (Type rep2)
    | rep1 == rep2 = []
    | otherwise = errorWithMeta x $ unifyErrorMessage rep1 rep2
  unify x (KArr l1 r1) (KArr l2 r2) = [WithMeta x (l1 :~ l2), WithMeta x (r1 :~ r2)]
  unify x k1 k2 = errorWithMeta x $ unifyErrorMessage k1 k2

type KindMap = Map KindVar (UTerm KindF KindVar)

newtype KindUnifyT m a = KindUnifyT {unKindUnifyT :: StateT KindMap m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadState KindMap, MonadUniq, MonadMalgo, MonadIO)

runKindUnifyT :: Monad m => KindUnifyT m a -> m a
runKindUnifyT (KindUnifyT m) = evalStateT m mempty

instance (Monad m, MonadUniq m) => MonadBind KindF KindVar (KindUnifyT m) where
  lookupVar v = Map.lookup v <$> get
  freshVar = KindVar <$> newLocalId "k" ()
  bindVar x v k = do
    occursCheck x v k
    modify (Map.insert v k)

class HasKind k t | t -> k where
  kindOf :: t -> k

instance HasKind UKind UKind where
  kindOf = id

instance HasKind Kind Kind where
  kindOf = id

bindUnknownToBoxed :: MonadBind KindF KindVar m => UTerm KindF KindVar -> m (UTerm KindF KindVar)
bindUnknownToBoxed term = do
  zonkedTerm <- zonkUTerm term
  let fvs = freevars zonkedTerm
  traverse_ (\fv -> bindVar () fv (UTerm $ Type BoxedRep)) fvs
  zonkUTerm zonkedTerm
