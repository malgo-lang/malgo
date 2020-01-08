{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Malgo.MiddleEnd.Closure
  ( Closure
  )
where

import           Data.Set                       ( intersection )
import           Language.Malgo.ID
import qualified Language.Malgo.IR.HIR         as H
import           Language.Malgo.IR.MIR         as M
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Pretty
import           Language.Malgo.TypeRep.Type
import           Language.Malgo.Prelude

data Closure

instance Pass Closure (H.Expr Type (ID Type)) (Program Type (ID Type)) where
  passName = "Closure"
  isDump   = dumpClosure
  trans e = do
    functionsRef <- newIORef []
    usingReaderT (Env [] [] functionsRef) $ do
      e' <- transExpr e
      fs <- readIORef functionsRef
      pure (Program fs e')

data Env = Env { knowns   :: [ID Type]
               , mutrecs  :: [ID Type]
               , functions :: IORef [Func Type (ID Type)]
               }

type TransM a = ReaderT Env MalgoM a

addFunc :: Func Type (ID Type) -> TransM ()
addFunc func = do
  Env { functions } <- ask
  modifyIORef functions (func :)

getFunc :: ID Type -> TransM (Func Type (ID Type))
getFunc func = do
  Env { functions } <- ask
  fs                <- readIORef functions
  case find (\Func { name } -> name == func) fs of
    Just f  -> pure f
    Nothing -> errorDoc $ "error(getFunc): function" <+> pPrint func <+> "is not defined"

transExpr :: H.Expr Type (ID Type) -> TransM (M.Expr Type (ID Type))
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
  Env { functions } <- ask
  newRef            <- newIORef =<< readIORef functions
  -- defsがすべてknownだと仮定してMIRに変換し、その自由変数を求める
  (e', fv)          <-
    local (\env -> env { knowns = funcNames <> knowns env, mutrecs = mempty, functions = newRef })
    $   mapM_ (transDef Nothing) defs
    >>  (,)
    <$> transExpr e
    -- 変換したdefsの自由変数を集計する
    <*> foldForM funcNames
                 (getFunc >=> \Func { params, body } -> pure $ freevars body \\ fromList params)
  if fv == mempty && (freevars e' `intersection` fromList funcNames) == mempty
    -- defsが自由変数を含まず、またdefsで宣言される関数がeの中で値として現れないならdefsはknownである
    then do
      writeIORef functions =<< readIORef newRef
      pure e'
    else do
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
    case captures of
      Nothing   -> pure mempty
      Just caps -> pure $ Endo $ Let name (MakeClosure name caps)

freevars :: Ord a => Expr t a -> Set a
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
