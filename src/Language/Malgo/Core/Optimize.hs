{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Malgo.Core.Optimize
  ( Optimize,
  )
where

import Language.Malgo.IR.Core
import Language.Malgo.Id
import Language.Malgo.Monad
import Language.Malgo.Pass
import Language.Malgo.Prelude
import Language.Malgo.TypeRep.CType

data Optimize

instance Pass Optimize (Exp (Id CType)) (Exp (Id CType)) where
  passName = "optimize"
  isDump = dumpDesugar
  trans = optVarBind

optVarBind :: (Eq a, Applicative f) => Exp a -> f (Exp a)
optVarBind (Match (Atom (Var x)) (Bind x' e :| [])) = fmap (\v -> if v == x' then x else v) <$> optVarBind e
optVarBind (Let ds e) = Let <$> traverse (rtraverse (appObj optVarBind)) ds <*> optVarBind e
optVarBind (Match v cs) = Match <$> optVarBind v <*> traverse (appCase optVarBind) cs
optVarBind e = pure e

appObj ::
  Applicative f =>
  (Exp a -> f (Exp a)) ->
  Obj a ->
  f (Obj a)
appObj f (Fun ps e) = Fun ps <$> f e
appObj _ o = pure o

appCase ::
  Functor f =>
  (Exp a -> f (Exp a)) ->
  Case a ->
  f (Case a)
appCase f (Unpack con ps e) = Unpack con ps <$> f e
appCase f (Bind x e) = Bind x <$> f e
