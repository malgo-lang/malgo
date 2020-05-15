{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Core.LambdaLift
  ( LambdaLift,
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.Malgo.IR.Core
import Language.Malgo.Id
import Language.Malgo.Monad
import Language.Malgo.Pass
import Language.Malgo.Prelude
import Language.Malgo.TypeRep.CType

data LambdaLift

data Env
  = Env
      { _toplevels :: Map (Id CType) (Obj (Id CType)),
        _knowns :: Set (Id CType)
      }

makeLenses ''Env

instance Pass LambdaLift (Exp (Id CType)) (Program (Id CType)) where
  passName = "lambda lift"
  isDump = dumpLambdaLift
  trans e = do
    (mainExpr, Env {_toplevels = ds}) <- runStateT (llift e) $ Env mempty mempty
    mainId <- newId (PackT [Con "Tuple0" []] :-> cTypeOf mainExpr) "$main"
    hole <- newId (PackT [Con "Tuple0" []]) "$hole"
    pure $ Program mainId ((mainId, Fun [hole] mainExpr) : Map.assocs ds)

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
      Fun xs call@Call {} -> Just . (n,) . Fun xs <$> llift call
      o@(Fun _ PrimCall {}) -> pure $ Just (n, o)
      o@(Fun _ CallDirect {}) -> pure $ Just (n, o)
      (Fun as body) -> do
        backup <- get
        ks <- use knowns
        -- nがknownだと仮定してlambda liftする
        knowns <>= Set.singleton n
        body' <- llift body
        toplevels %= Map.insert n (Fun as body')
        e' <- llift e
        -- (Fun as body')の自由変数がknownsとnを除いてなく、e'の自由変数にnが含まれないならnはknown
        let fvs = freevars body' Set.\\ (ks <> Set.fromList as)
        if null (sans n fvs) && n `notElem` freevars e'
          then pure Nothing
          else do
            put backup
            newFun <- def (n ^. idName) (toList fvs <> as) =<< llift body
            pure $ Just (n, Fun as (CallDirect newFun $ map Var $ toList fvs <> as))
      o -> pure $ Just (n, o)
llift (Match e cs) =
  Match <$> llift e <*> traverse aux cs
  where
    aux (Unpack con ps body) = Unpack con ps <$> llift body
    aux (Bind x body) = Bind x <$> llift body
llift e = pure e

def ::
  ( MonadUniq m,
    MonadState Env m
  ) =>
  String ->
  [Id CType] ->
  Exp (Id CType) ->
  m (Id CType)
def name xs e = do
  f <- newId (foldr ((:->) . cTypeOf) (cTypeOf e) xs) ("$" <> name)
  toplevels %= Map.insert f (Fun xs e)
  pure f
