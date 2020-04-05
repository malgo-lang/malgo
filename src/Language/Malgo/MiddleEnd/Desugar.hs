{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Malgo.MiddleEnd.Desugar (Desugar) where

import qualified Data.Text                    as T
import           Language.Malgo.Id
import           Language.Malgo.IR.Core
import qualified Language.Malgo.IR.Syntax     as S
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Prelude
import           Language.Malgo.TypeRep.CType
import           Language.Malgo.TypeRep.Type

data Desugar

instance Pass Desugar (S.Expr (Id Type)) (Exp (Id CType)) where
  passName = "Desugar"
  isDump = const False -- TODO: dumpDesugar
  trans e = evalStateT ?? mempty $ toExp e

newTmp :: MonadUniq m => CType -> m (Id CType)
newTmp t = newId t "$d"

findVar :: MonadState (IdMap Type (Id CType)) m => Id Type -> m (Id CType)
findVar = undefined

def :: (MonadUniq f, MonadWriter (Endo (Exp (Id CType))) f) => Exp (Id CType) -> f (Id CType)
def (Atom (Var x)) = pure x
def v = do
  x <- newTmp (cTypeOf v)
  tell $ Endo $ \e -> Match v [] (x, e)
  pure x

runDef :: Functor f => WriterT (Endo a) f a -> f a
runDef m = uncurry (flip appEndo) <$> runWriterT m

toExp :: (MonadState (IdMap Type (Id CType)) f, MonadUniq f) => S.Expr (Id Type) -> f (Exp (Id CType))
toExp (S.Var _ x) = do
  Atom . Var <$> findVar x
toExp (S.Int _ x) = do
  v <- newTmp $ PackT "Int" [IntT]
  pure $ Let [(v, Pack (Con "Int" 1) [Unboxed (Int x)])] $ Atom $ Var v
toExp (S.Float _ x) = do
  v <- newTmp $ PackT "Float" [FloatT]
  pure $ Let [(v, Pack (Con "Float" 1) [Unboxed (Float x)])] $ Atom $ Var v
toExp (S.Bool _ x) = do
  v <- newTmp $ PackT "Bool" []
  let o = if x then Pack (Con "True" 0) [] else Pack (Con "False" 0) []
  pure $ Let [(v, o)] $ Atom $ Var v
toExp (S.Char _ x) = do
  v <- newTmp $ PackT "Char" [CharT]
  pure $ Let [(v, Pack (Con "Char" 1) [Unboxed $ Char x])] $ Atom $ Var v
toExp (S.String _ x) = do
  v <- newTmp $ PackT "String" [StringT]
  pure $ Let [(v, Pack (Con "String" 1) [Unboxed $ String x])] $ Atom $ Var v
toExp (S.Tuple _ xs) = runDef $ do
  xs' <- traverse toExp xs
  vs <- traverse def xs'
  v <- newTmp $ PackT ("Tuple" <> T.pack (show $ length xs)) (map cTypeOf vs)
  pure $ Let [(v, Pack (Con ("Tuple" <> T.pack (show $ length xs)) $ length xs) (map Var vs))] $ Atom $ Var v
