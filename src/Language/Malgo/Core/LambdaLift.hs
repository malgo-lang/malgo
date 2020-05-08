{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Core.LambdaLift
  ( LambdaLift,
  )
where

import qualified Data.Map as Map
import Language.Malgo.IR.Core
import Language.Malgo.Id
import Language.Malgo.Monad
import Language.Malgo.Pass
import Language.Malgo.Prelude
import Language.Malgo.TypeRep.CType

data LambdaLift

instance Pass LambdaLift (Exp (Id CType)) (Program (Id CType)) where
  passName = "lambda lift"
  isDump = dumpLambdaLift
  trans e = do
    (mainExpr, ds) <- runStateT (llift e) mempty
    mainId <- newId (PackT [Con "Tuple0" []] :-> cTypeOf mainExpr) "$main"
    hole <- newId (PackT [Con "Tuple0" []]) "$hole"
    pure $ Program mainId ((mainId, Fun [hole] mainExpr) : Map.assocs ds)

llift ::
  ( MonadUniq f,
    MonadState (Map (Id CType) (Obj (Id CType))) f
  ) =>
  Exp (Id CType) ->
  f (Exp (Id CType))
llift (Let ds e) =
  Let <$> traverse aux ds <*> llift e
  where
    aux (n, v) = case v of
      o@(Fun as body) -> do
        let fvs = toList $ sans n (freevars o)
        -- if null fvs
        --   then (n,) <$> (Fun as <$> llift body)
        --   else do
        newFun <- newId (foldr ((:->) . cTypeOf) (cTypeOf body) (fvs <> as)) ("$" <> (n ^. idName))
        body' <- Let [(n, Fun as (Call newFun $ map Var fvs))] <$> llift body
        modify $ Map.insert newFun (Fun (fvs <> as) body')
        pure (n, Fun as (Call newFun $ map Var fvs))
      o -> pure (n, o)
llift (Match e cs) =
  Match <$> llift e <*> traverse aux cs
  where
    aux (Unpack con ps body) = Unpack con ps <$> llift body
    aux (Bind x body) = Bind x <$> llift body
llift e = pure e
