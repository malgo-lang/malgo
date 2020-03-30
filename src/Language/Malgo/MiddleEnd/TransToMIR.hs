{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.MiddleEnd.TransToMIR
  ( TransToMIR
  )
where

import           Language.Malgo.Id
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Prelude
import           Language.Malgo.Pretty

import           Language.Malgo.IR.MIR         as M
import qualified Language.Malgo.IR.HIR         as H

import           Language.Malgo.TypeRep.Type

import           Control.Lens.Indexed           ( ifoldMap )
import           Control.Lens.At                ( at )
import           Control.Lens.Setter            ( (?~) )
import           Control.Lens.Getter            ( (^.) )
import           Data.Set                       ( intersection
                                                , (\\)
                                                )
import qualified Data.Set                      as Set

data TransToMIR

instance Pass TransToMIR (H.Expr (Id Type)) (Program (Id Type)) where
  passName = "TransToMIR"
  isDump   = dumpClosure
  trans e = evalStateT ?? mempty $ runReaderT ?? (Env [] []) $ do
    e' <- transExpr e
    fs <- gets toList
    pure (Program fs e')

data Env = Env { knowns   :: [Id Type]
               , mutrecs  :: [Id Type]
               }

type FuncMap = Map (Id Type) (Func (Id Type))

addFunc :: MonadState FuncMap m => Func (Id Type) -> m ()
addFunc func = modify (at (M.name func) ?~ func)

getFunc :: MonadState FuncMap m => Id Type -> m (Func (Id Type))
getFunc func = do
  fs <- get
  case fs ^. at func of
    Just f  -> pure f
    Nothing -> errorDoc $ "error(getFunc): function" <+> pPrint func <+> "is not defined"

transExpr :: (MonadState FuncMap f, MonadReader Env f) => H.Expr (Id Type) -> f (Expr (Id Type))
transExpr (H.Var   x              ) = pure $ Var x
transExpr (H.Lit   x              ) = pure $ Lit x
transExpr (H.Tuple xs             ) = pure $ Tuple xs
transExpr (H.TupleAccess x   i    ) = pure $ TupleAccess x i
transExpr (H.MakeArray   x   n    ) = pure $ MakeArray x n
transExpr (H.ArrayRead   arr ix   ) = pure $ ArrayRead arr ix
transExpr (H.ArrayWrite arr ix val) = pure $ ArrayWrite arr ix val
transExpr (H.Call f xs            ) = do
  Env { knowns, mutrecs } <- ask
  pure $ if
    | -- 直接呼び出せる関数はCallDirect
      f `elem` knowns  -> CallDirect f xs
    | -- (相互)再帰している関数はCallWithCaptures
      f `elem` mutrecs -> CallWithCaptures f xs
    | -- それ以外はCallClosure
      otherwise        -> CallClosure f xs
transExpr (H.Let   x    v  e ) = Let x <$> transExpr v <*> transExpr e
transExpr (H.If    c    t  f ) = If c <$> transExpr t <*> transExpr f
transExpr (H.Prim  orig ty xs) = pure $ Prim orig ty xs
transExpr (H.BinOp op   x  y ) = pure $ BinOp op x y
transExpr (H.LetRec defs e   ) = do
  backup <- get
  -- defsがすべてknownだと仮定してMIRに変換し、その自由変数を求める
  e'     <- local (\env -> env { knowns = funcNames <> knowns env, mutrecs = mempty }) $ do
    mapM_ (transDef Nothing) defs
    transExpr e
  -- 変換したdefsの自由変数を集計
  fv <- foldMapA
    (getFunc >=> \Func { params, body } -> pure $ freevars body \\ Set.fromList params)
    funcNames
  -- defsが自由変数を含まず、またdefsで宣言される関数がeの中で値として現れないならdefsはknownである
  if null fv && null (freevars e' `intersection` Set.fromList funcNames)
    then pure e'
    else do
      put backup
      -- 自由変数をcapturesに、相互再帰しうる関数名をmutrecsに入れてMIRに変換する
      defs' <- local (\env -> (env :: Env) { mutrecs = funcNames })
        $ foldMapA (transDef (Just $ toList $ fv \\ Set.fromList funcNames)) defs
      appEndo defs' <$> transExpr e
 where
  funcNames = map H.name defs
  transDef captures H.Def { name, params, expr } = do
    expr'           <- transExpr expr
    Env { mutrecs } <- ask
    addFunc $ Func { name     = name
                   , captures = captures
                   , mutrecs  = mutrecs
                   , params   = params
                   , body     = expr'
                   }
    pure $ foldMap (Endo . Let name . MakeClosure name) captures
transExpr (H.Match s ((H.VarP x, e) :| _)) = Let x (Var s) <$> transExpr e
transExpr (H.Match s ((H.TupleP xs, e) :| _)) =
  appEndo (ifoldMap (\i x -> Endo (Let x (TupleAccess s i))) xs) <$> transExpr e

freevars :: Ord a => Expr a -> Set a
freevars (Var x)                 = Set.singleton x
freevars Lit{}                   = mempty
freevars (Tuple xs             ) = Set.fromList xs
freevars (TupleAccess x _      ) = Set.singleton x
freevars (MakeArray   x n      ) = Set.fromList [x, n]
freevars (ArrayRead   x y      ) = Set.fromList [x, y]
freevars (ArrayWrite x y z     ) = Set.fromList [x, y, z]
freevars (CallDirect       _ xs) = Set.fromList xs
freevars (CallWithCaptures _ xs) = Set.fromList xs
freevars (CallClosure      f xs) = Set.fromList $ f : xs
freevars (MakeClosure      _ xs) = Set.fromList xs
freevars (Let   n v e          ) = Set.delete n (freevars v <> freevars e)
freevars (If    c t f          ) = Set.singleton c <> freevars t <> freevars f
freevars (Prim  _ _ xs         ) = Set.fromList xs
freevars (BinOp _ x y          ) = Set.fromList [x, y]
