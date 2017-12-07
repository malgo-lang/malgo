{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Malgo.Beta where

import           Control.Monad.State
import           Data.Maybe             (fromMaybe)
import qualified Language.Malgo.KNormal as K
import           Language.Malgo.Utils

newtype BetaTransState = BetaTransState { table :: [(Name, Name)] }
  deriving Show

newtype BetaTrans a = BetaTrans (StateT BetaTransState (Either String) a)
  deriving (Functor, Applicative, Monad, MonadState BetaTransState)

runBetaTrans :: BetaTrans a -> Either String a
runBetaTrans (BetaTrans m) = evalStateT m (BetaTransState [])

betaTrans :: K.Expr -> Either String K.Expr
betaTrans e = runBetaTrans (trans e)

addBind :: (Name, Name) -> BetaTrans ()
addBind (x, y) =
  modify $ \e -> e { table = (x, y) : table e }

find :: Name -> BetaTrans Name
find x = do
  env <- gets table
  let x' = lookup x env
  return $ fromMaybe x x'

trans :: K.Expr -> BetaTrans K.Expr
trans (K.Let (K.FunDec fn params retTy body) lbody, ty) =
  (,) <$> (K.Let
            <$> (K.FunDec fn params retTy
                  <$> trans body)
            <*> trans lbody)
  <*> pure ty
trans (K.Let (K.ValDec name typ val) body, ty) = do
  val' <- trans val
  case val' of
    (K.Var x, _) ->
      addBind (name, x) >> trans body
    _ ->
      (,)
      <$> (K.Let (K.ValDec name typ val') <$> trans body)
      <*> pure ty
trans (K.BinOp op x y, ty) =
  (,) <$> (K.BinOp op
           <$> ((,) <$> find (fst x) <*> pure (snd x))
           <*> ((,) <$> find (fst y) <*> pure (snd y)))
  <*> pure ty
trans (K.If c t f, ty) =
  (,) <$> (K.If <$> ((,) <$> find (fst c) <*> pure (snd c))
           <*> trans t <*> trans f) <*> pure ty
trans (K.Call fn args, ty) =
  (,) <$> (K.Call
           <$> ((,) <$> find (fst fn) <*> pure (snd fn))
           <*> mapM (\a -> (,) <$> find (fst a) <*> pure (snd a)) args)
  <*> return ty
trans (K.Var x, ty) =
  (,) <$> (K.Var <$> find x) <*> pure ty
trans x = return x
