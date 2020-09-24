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
  ( LambdaLift, lambdalift
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
  { _funcs :: Map (Id CType) ([Id CType], Exp (Id CType)),
    _knowns :: Set (Id CType)
  }

makeLensesFor [("_funcs", "funcs"), ("_knowns", "knowns")] ''Env

instance Pass LambdaLift (Program (Id CType)) (Program (Id CType)) where
  passName = "lambda lift"
  trans = lambdalift

lambdalift :: MonadUniq m => Program (Id CType) -> m (Program (Id CType))
lambdalift Program {mainExp, topFuncs} = evalStateT
    ?? Env
      { 
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
      Env {_funcs} <- get
      traverseOf appProgram (pure . flat) $ Program (Map.assocs _funcs) mainExp

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
llift (Match e cs) =
  Match <$> llift e <*> traverseOf (traversed % appCase) llift cs
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
  modifying funcs $ Map.insert f (xs, e)
  pure f
