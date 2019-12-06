{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
module Language.Malgo.MiddleEnd.New.Closure ( Closure ) where

import           Data.Set                          (intersection)
import           Language.Malgo.ID
import qualified Language.Malgo.IR.HIR             as H
import           Language.Malgo.IR.LIR             as L
import           Language.Malgo.MiddleEnd.FreeVars
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.TypeRep.Type
import           Relude                            hiding (Type)

data Closure

instance Pass Closure (H.Expr Type TypedID) (Program Type TypedID) where
  isDump = dumpClosure
  trans e = evaluatingStateT [] $ usingReaderT (Env [] mempty []) $ do
    e' <- transExpr e
    fs <- get
    pure (Program fs e')

data Env = Env { knowns   :: [TypedID]
               , captures :: Set TypedID
               , mutrecs  :: [TypedID]
               }

type TransM a = ReaderT Env (StateT [Func Type TypedID] MalgoM) a

addFunc :: MonadState [Func t a] m => Func t a -> m ()
addFunc func = modify (func:)

transExpr :: H.Expr Type (ID Type) -> TransM (L.Expr Type TypedID)
transExpr (H.Var x) = pure $ Var x
transExpr (H.Lit x) = pure $ Lit x
transExpr (H.Tuple xs) = pure $ Tuple xs
transExpr (H.TupleAccess x i) = pure $ TupleAccess x i
transExpr (H.MakeArray ty x) = pure $ MakeArray ty x
transExpr (H.ArrayRead arr ix) = pure $ ArrayRead arr ix
transExpr (H.ArrayWrite arr ix val) = pure $ ArrayWrite arr ix val
transExpr (H.Call f xs) = do
  Env { knowns, mutrecs } <- ask
  pure $ if | f `elem` knowns -> CallDir f xs -- 直接呼び出せる関数はCallDir
            | f `elem` mutrecs -> CallWithCaptures f xs -- (相互)再帰している関数はCallWithCaptures
            | otherwise -> CallCls f xs -- それ以外はCallCls
transExpr (H.Let x v e) = Let x <$> transExpr v <*> transExpr e
transExpr (H.If c t f) = If c <$> transExpr t <*> transExpr f
transExpr (H.Prim orig ty) = pure $ Prim orig ty
transExpr (H.BinOp op x y) = pure $ BinOp op x y
transExpr (H.LetRec defs e)
  | fv == mempty && (freevars e `intersection` fromList funcNames) == mempty = do
      _ <- local (\env -> (env :: Env) { knowns = funcNames <> knowns env, captures = mempty, mutrecs = mempty }) (transDefs defs)
      local (\env -> env { knowns = funcNames <> knowns env }) $ transExpr e
  | otherwise = do
      defs' <- local (\env -> (env :: Env) { captures = fv, mutrecs = map (\(f, _, _) -> f) defs }) (transDefs defs)
      defs' <$> transExpr e
  where
    funcNames = map (\(f, _, _) -> f) defs
    fv = mconcat $ map (\(_, ps, b) -> freevars b \\ fromList ps) defs
    transDefs []              = pure id
    transDefs ((f, xs, b):ds) = do
      b' <- transExpr b
      Env { captures } <- ask
      addFunc (Func { name = f, captures = toList captures, params = xs, body = b' })
      k <- transDefs ds
      pure (k . Let f (MakeCls f $ toList captures))
