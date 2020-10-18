{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Koriel.Core.LambdaLift
  ( lambdalift,
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Koriel.Core.Core
import Koriel.Core.Flat
import Koriel.Core.Type
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude

data Env = Env
  { _funcs :: Map (Id Type) ([Id Type], Exp (Id Type)),
    _knowns :: Set (Id Type)
  }

funcs :: Lens' Env (Map (Id Type) ([Id Type], Exp (Id Type)))
funcs = lens _funcs (\e x -> e { _funcs = x })

knowns :: Lens' Env (Set (Id Type))
knowns = lens _knowns (\e x -> e { _knowns = x })

lambdalift :: MonadUniq m => Program (Id Type) -> m (Program (Id Type))
lambdalift Program {mainExp, topFuncs} =
  evalStateT ?? Env {_funcs = mempty, _knowns = Set.fromList $ map fst topFuncs} $ do
    topFuncs <- traverse (\(f, (ps, e)) -> (f,) . (ps,) <$> llift e) topFuncs
    modify $ \env ->
      env
        { _funcs = _funcs env <> Map.fromList topFuncs,
          _knowns = _knowns env <> Set.fromList (Map.keys $ _funcs env)
        }
    mainExp <- llift mainExp
    Env {_funcs} <- get
    traverseOf appProgram (pure . flat) $ Program (Map.assocs _funcs) mainExp

llift :: (MonadUniq f, MonadState Env f) => Exp (Id Type) -> f (Exp (Id Type))
llift (Call (Var f) xs) = do
  ks <- use knowns
  if f `elem` ks then pure $ CallDirect f xs else pure $ Call (Var f) xs
llift (Let [(n, Fun xs call@Call {})] e) = do
  call' <- llift call
  Let [(n, Fun xs call')] <$> llift e
llift (Let [(n, o@(Fun _ PrimCall {}))] e) = Let [(n, o)] <$> llift e
llift (Let [(n, o@(Fun _ CallDirect {}))] e) = Let [(n, o)] <$> llift e
llift (Let [(n, Fun as body)] e) = do
  backup <- get
  ks <- use knowns
  -- nがknownだと仮定してlambda liftする
  modifying knowns $ Set.insert n
  body' <- llift body
  modifying funcs $ Map.insert n (as, body')
  (e', _) <- localState $ llift e
  -- (Fun as body')の自由変数がknownsを除いてなく、e'の自由変数にnが含まれないならnはknown
  -- (Call n _)は(CallDirect n _)に変換されているので、nが値として使われているときのみ自由変数になる
  let fvs = freevars body' Set.\\ (ks <> Set.fromList as)
  if null fvs && n `notElem` freevars e'
    then llift e
    else do
      put backup
      body' <- llift body
      let fvs = freevars body' Set.\\ (ks <> Set.fromList as)
      newFun <- def (n ^. idName) (toList fvs <> as) body'
      Let [(n, Fun as (CallDirect newFun $ map Var $ toList fvs <> as))] <$> llift e
llift (Let ds e) = Let ds <$> llift e
llift (Match e cs) = Match <$> llift e <*> traverseOf (traversed . appCase) llift cs
llift e = pure e

def :: (MonadUniq m, MonadState Env m) => String -> [Id Type] -> Exp (Id Type) -> m (Id Type)
def name xs e = do
  f <- newId (map typeOf xs :-> typeOf e) ("$" <> name)
  modifying funcs $ Map.insert f (xs, e)
  pure f
