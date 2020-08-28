{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Core.LambdaLift
  ( LambdaLift,
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.Malgo.Core.Flat
import Language.Malgo.IR.Core
import Language.Malgo.Id
import Language.Malgo.Monad
import Language.Malgo.Pass
import Language.Malgo.Prelude
import Language.Malgo.TypeRep.CType

data LambdaLift

data Env = Env
  { _binds :: Map (Id CType) (Obj (Id CType)),
    _funcs :: Map (Id CType) ([Id CType], Exp (Id CType)),
    _knowns :: Set (Id CType)
  }

makeLensesFor [("_funcs", "funcs"), ("_knowns", "knowns")] ''Env

instance Pass LambdaLift (Program (Id CType)) (Program (Id CType)) where
  passName = "lambda lift"
  trans Program {topBinds, mainExp, topFuncs} = evalStateT
    ?? Env
      { _binds = Map.fromList topBinds,
        _funcs = mempty,
        _knowns = Set.fromList $ map fst topFuncs
      }
    $ do
      topFuncs <- traverse (\(f, (ps, e)) -> (f,) . (ps,) <$> llift e) topFuncs
      modify $ \env ->
        env
          { _funcs = _funcs env <> Map.fromList topFuncs,
            _knowns = _knowns env <> Set.fromList (Map.keys $ _funcs env)
          }
      mainExp <- llift mainExp
      Env {_binds, _funcs} <- get
      lift $ appProgram (trans @Flat) $ Program (Map.assocs _binds) (Map.assocs _funcs) mainExp

llift ::
  ( MonadUniq f,
    MonadState Env f
  ) =>
  Exp (Id CType) ->
  f (Exp (Id CType))
llift (Call (Var f) xs) = do
  ks <- use knowns
  if f `elem` ks
    then pure $ CallDirect f xs
    else pure $ Call (Var f) xs
llift (Let ds e) = do
  ds' <- catMaybes <$> traverse aux ds
  case ds' of
    [] -> llift e
    _ -> Let ds' <$> llift e
  where
    aux (n, v) = case v of
      -- すでにクロージャになっているものはそのまま返す
      Fun xs call@Call {} -> Just . (n,) . Fun xs <$> llift call
      o@(Fun _ PrimCall {}) -> pure $ Just (n, o)
      o@(Fun _ CallDirect {}) -> pure $ Just (n, o)
      (Fun as body) -> do
        backup <- get
        ks <- use knowns
        -- nがknownだと仮定してlambda liftする
        knowns <>= Set.singleton n
        body' <- llift body
        funcs %= Map.insert n (as, body')
        (e', _) <- localState $ llift e
        -- (Fun as body')の自由変数がknownsを除いてなく、e'の自由変数にnが含まれないならnはknown
        -- (Call n _)は(CallDirect n _)に変換されているので、nが値として使われているときのみ自由変数になる
        let fvs = freevars body' Set.\\ (ks <> Set.fromList as)
        if null fvs && n `notElem` freevars e'
          then pure Nothing
          else do
            put backup
            body' <- llift body
            let fvs = freevars body' Set.\\ (ks <> Set.fromList as)
            newFun <- def (n ^. idName) (toList fvs <> as) body'
            pure $ Just (n, Fun as (CallDirect newFun $ map Var $ toList fvs <> as))
      o -> pure $ Just (n, o)
llift (Match e cs) =
  Match <$> llift e <*> traverse (appCase llift) cs
llift e = pure e

def ::
  ( MonadUniq m,
    MonadState Env m
  ) =>
  Text ->
  [Id CType] ->
  Exp (Id CType) ->
  m (Id CType)
def name xs e = do
  f <- newId (map cTypeOf xs :-> cTypeOf e) ("$" <> name)
  funcs %= Map.insert f (xs, e)
  pure f
