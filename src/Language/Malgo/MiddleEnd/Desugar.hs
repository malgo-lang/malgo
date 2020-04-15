{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
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

def :: (MonadUniq f, MonadWriter (Endo (Exp (Id CType))) f) => Exp (Id CType) -> f (Atom (Id CType))
def (Atom x) = pure x
def v = do
  x <- newTmp (cTypeOf v)
  tell $ Endo $ \e -> Match v [] (x, e)
  pure (Var x)

runDef :: Functor f => WriterT (Endo a) f a -> f a
runDef m = uncurry (flip appEndo) <$> runWriterT m

toExp :: (MonadState (IdMap Type (Id CType)) f, MonadUniq f) => S.Expr (Id Type) -> f (Exp (Id CType))
toExp (S.Var _ x) =
  Atom . Var <$> findVar x
toExp (S.Int _ x) =
  let_ (Pack (Con "Int" [IntT]) [Unboxed (Int x)]) $ pure . Atom . Var
toExp (S.Float _ x) =
  let_ (Pack (Con "Float" [FloatT]) [Unboxed (Float x)]) $ pure . Atom . Var
toExp (S.Bool _ x) =
  let_ (if x then Pack (Con "True" []) [] else Pack (Con "False" []) []) $ pure . Atom . Var
toExp (S.Char _ x) =
  let_ (Pack (Con "Char" [CharT]) [Unboxed $ Char x]) $ pure . Atom . Var
toExp (S.String _ x) =
  let_ (Pack (Con "String" [StringT]) [Unboxed $ String x]) $ pure . Atom . Var
toExp (S.Tuple _ xs) = runDef $ do
  vs <- traverse (def <=< toExp) xs
  let_ (Pack (Con ("Tuple" <> T.pack (show $ length xs)) $ map cTypeOf vs) vs) $ pure . Atom . Var
toExp (S.Array _ (x :| xs)) = runDef $ do
  x' <- def =<< toExp x
  let_ (Array x' $ Unboxed (Int $ fromIntegral $ length xs + 1)) $ \arr -> do
    ifor_ xs $ \i v -> do
      v' <- def =<< toExp v
      def (ArrayWrite (Var arr) (Unboxed (Int $ fromIntegral $ i + 1)) v')
    pure $ Atom $ Var arr
toExp (S.MakeArray _ a n) = runDef $ do
  a' <- def =<< toExp a
  n' <- def =<< toExp n
  v <- newTmp $ ArrayT (cTypeOf a')
  match (Atom n') [(Con "Int" [IntT], \[n''] -> pure $ Let [(v, Array a' $ Var n'')] $ Atom a')] (\_ -> pure Undefined)
toExp (S.ArrayRead _ a i) = runDef $ do
  a' <- def =<< toExp a
  i' <- def =<< toExp i
  match (Atom i') [(Con "Int" [IntT], \[i''] -> Atom <$> def (ArrayRead a' $ Var i''))] (\_ -> pure Undefined)
toExp (S.ArrayWrite _ a i x) = runDef $ do
  a' <- def =<< toExp a
  i' <- def =<< toExp i
  x' <- def =<< toExp x
  match (Atom i') [(Con "Int" [IntT], \[i''] -> Atom <$> def (ArrayWrite a' (Var i'') x'))] (\_ -> pure Undefined)
toExp (S.Call _ f xs) = runDef $ do
  f' <- def =<< toExp f
  xs' <- traverse (def <=< toExp) xs
  let_ (Pack (Con ("Tuple" <> T.pack (show $ length xs)) $ map cTypeOf xs') xs') $ \arg -> case f' of
    Var fun -> pure $ Call fun [Var arg]
    _       -> bug Unreachable

let_ ::
  MonadUniq m
  => Obj (Id CType)
  -> (Id CType -> WriterT (Endo (Exp (Id CType))) m (Exp (Id CType)))
  -> m (Exp (Id CType))
let_ o body = do
  v <- newTmp $ cTypeOf o
  body' <- runDef $ body v
  pure $ Let [(v, o)] body'

match ::
  MonadUniq m
  => Exp (Id CType)
  -> [(Con, [Id CType] -> WriterT (Endo (Exp (Id CType))) m (Exp (Id CType)))]
  -> (Id CType -> WriterT (Endo (Exp (Id CType))) m (Exp (Id CType)))
  -> m (Exp (Id CType))
match e ps d = do
  hole <- newTmp $ cTypeOf e
  ps' <- traverse ?? ps $ \(con@(Con _ ts), body) -> do
    vs <- traverse newTmp ts
    body' <- runDef $ body vs
    pure $ Unpack con vs body'
  d' <- runDef $ d hole
  pure $ Match e ps' (hole, d')
