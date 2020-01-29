{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Malgo.MiddleEnd.TransToMIR
  ( TransToMIR
  )
where

import           Control.Lens.Indexed           ( ifoldMap )
import           Data.Set                       ( intersection )
import           Language.Malgo.ID
import qualified Language.Malgo.IR.HIR         as H
import           Language.Malgo.IR.MIR         as M
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Pretty
import           Language.Malgo.TypeRep.Type
import           Language.Malgo.Prelude

data TransToMIR

instance Pass TransToMIR (H.Expr (ID Type)) (Program (ID Type)) where
  passName = "TransToMIR"
  isDump   = dumpClosure
  trans e = evaluatingStateT [] $ usingReaderT (Env [] []) $ do
    e' <- transExpr e
    fs <- get
    pure (Program fs e')

data Env = Env { knowns   :: [ID Type]
               , mutrecs  :: [ID Type]
               }

type Defined = [Func (ID Type)]

type TransM a = ReaderT Env (StateT Defined MalgoM) a

addFunc :: Func (ID Type) -> TransM ()
addFunc func = modify (func :)

getFunc :: ID Type -> TransM (Func (ID Type))
getFunc func = do
  fs <- get
  case find (\Func { name } -> name == func) fs of
    Just f  -> pure f
    Nothing -> errorDoc $ "error(getFunc): function" <+> pPrint func <+> "is not defined"

transExpr :: H.Expr (ID Type) -> TransM (M.Expr (ID Type))
transExpr (H.Var   x              ) = pure $ Var x
transExpr (H.Lit   x              ) = pure $ Lit x
transExpr (H.Tuple xs             ) = pure $ Tuple xs
transExpr (H.TupleAccess x    i   ) = pure $ TupleAccess x i
transExpr (H.MakeArray   init size) = pure $ MakeArray init size
transExpr (H.ArrayRead   arr  ix  ) = pure $ ArrayRead arr ix
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
  backup   <- get
  -- defsがすべてknownだと仮定してMIRに変換し、その自由変数を求める
  (e', fv) <- local (\env -> env { knowns = funcNames <> knowns env, mutrecs = mempty }) $ do
    mapM_ (transDef Nothing) defs
    (,)
      <$> transExpr e
      -- 変換したdefsの自由変数を集計する
      <*> foldMapM (getFunc >=> \Func { params, body } -> pure $ freevars body \\ fromList params)
                   funcNames
  if fv == mempty && (freevars e' `intersection` fromList funcNames) == mempty
    -- defsが自由変数を含まず、またdefsで宣言される関数がeの中で値として現れないならdefsはknownである
    then pure e'
    else do
      put backup
      -- 自由変数をcapturesに、相互再帰しうる関数名をmutrecsに入れてMIRに変換する
      defs' <- local (\env -> (env :: Env) { mutrecs = funcNames })
        $ foldMapM (transDef $ Just $ toList $ fv \\ fromList funcNames) defs
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
freevars (Var x)                 = one x
freevars Lit{}                   = mempty
freevars (Tuple xs             ) = fromList xs
freevars (TupleAccess x    _   ) = one x
freevars (MakeArray   init size) = fromList [init, size]
freevars (ArrayRead   x    y   ) = fromList [x, y]
freevars (ArrayWrite x y z     ) = fromList [x, y, z]
freevars (CallDirect       _ xs) = fromList xs
freevars (CallWithCaptures _ xs) = fromList xs
freevars (CallClosure      f xs) = fromList $ f : xs
freevars (MakeClosure      _ xs) = fromList xs
freevars (Let   n v e          ) = delete n (freevars v <> freevars e)
freevars (If    c t f          ) = one c <> freevars t <> freevars f
freevars (Prim  _ _ xs         ) = fromList xs
freevars (BinOp _ x y          ) = fromList [x, y]
