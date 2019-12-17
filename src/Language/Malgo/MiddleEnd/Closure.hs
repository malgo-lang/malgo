{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Malgo.MiddleEnd.Closure ( Closure ) where

import           Data.Set                          (intersection)
import           Language.Malgo.ID
import qualified Language.Malgo.IR.HIR             as H
import           Language.Malgo.IR.MIR             as M
import           Language.Malgo.MiddleEnd.FreeVars
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Pretty
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
               , captures :: Maybe [TypedID]
               , mutrecs  :: [TypedID]
               }

type TransM a = ReaderT Env (StateT [Func Type TypedID] MalgoM) a

addFunc :: MonadState [Func t a] m => Func t a -> m ()
addFunc func = modify (func:)

getFunc :: (MonadState [Func Type TypedID] m, MonadIO m) =>
  TypedID -> m (Func Type TypedID)
getFunc func = do
  env <- get
  case find (\Func{name} -> name == func) env of
    Just f -> pure f
    Nothing -> errorDoc $ "error(getFunc): function" <+> pPrint func <+> "is not defined"

transExpr :: H.Expr Type (ID Type) -> TransM (M.Expr Type TypedID)
transExpr (H.Var x) = pure $ Var x
transExpr (H.Lit x) = pure $ Lit x
transExpr (H.Tuple xs) = pure $ Tuple xs
transExpr (H.TupleAccess x i) = pure $ TupleAccess x i
transExpr (H.MakeArray ty x) = pure $ MakeArray ty x
transExpr (H.ArrayRead arr ix) = pure $ ArrayRead arr ix
transExpr (H.ArrayWrite arr ix val) = pure $ ArrayWrite arr ix val
transExpr (H.Call f xs) = do
  Env { knowns, mutrecs } <- ask
  pure $ if | f `elem` knowns -> CallDirect f xs -- 直接呼び出せる関数はCallDir
            | f `elem` mutrecs -> CallWithCaptures f xs -- (相互)再帰している関数はCallWithCaptures
            | otherwise -> CallClosure f xs -- それ以外はCallCls
transExpr (H.Let x v e) = do
  v' <- transExpr v
  Let [(x, v')] <$> transExpr e
transExpr (H.If c t f) = If c <$> transExpr t <*> transExpr f
transExpr (H.Prim orig ty xs) = pure $ Prim orig ty xs
transExpr (H.BinOp op x y) = pure $ BinOp op x y
transExpr (H.LetRec defs e) = do
  envBackup <- get
  fv <- getFreeVars
  if | fv == mempty && (freevars e `intersection` fromList funcNames) == mempty ->
         local (\env -> env { knowns = funcNames <> knowns env, captures = Nothing }) $ transExpr e
     | otherwise -> do
         put envBackup
         defs' <- local (\env -> (env :: Env) { captures = Just $ toList (fv \\ fromList funcNames), mutrecs = funcNames }) (transDefs defs)
         Let defs' <$> transExpr e
  where
    funcNames = map (\(f, _, _) -> f) defs
    getFreeVars = do
      _ <- local (\env -> (env :: Env) { knowns = funcNames <> knowns env, captures = Nothing, mutrecs = mempty }) (transDefs defs)
      mconcat <$> forM funcNames (\f -> do
                                     Func {params, body} <- getFunc f
                                     pure $ freevars body \\ fromList params)
    transDefs []              = pure []
    transDefs ((f, xs, b):ds) = do
      b' <- transExpr b
      Env { captures } <- ask
      addFunc (Func { name = f, captures = captures, mutrecs = funcNames, params = xs, body = b' })
      ks <- transDefs ds
      case captures of
        Just caps -> pure ((f, MakeClosure f caps) : ks)
        Nothing   -> pure ks
