{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Language.Malgo.KNormal where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map.Strict          as Map
import           Data.String
import           Language.Malgo.Rename
import           Language.Malgo.Syntax
import           Language.Malgo.TypeCheck
import           Language.Malgo.Utils
import           Text.PrettyPrint

newtype KEnv = KEnv { _count :: Int }

initKEnv :: RnEnv -> KEnv
initKEnv (RnEnv c _ _) = KEnv { _count = c }

type KNormal a = Malgo KEnv a

runKNormal :: RnEnv -> KNormal a -> (Either MalgoError a, KEnv)
runKNormal e m = runMalgo m (initKEnv e)
knormal :: RnEnv -> Expr TypedID -> (Either MalgoError (Expr TypedID), KEnv)
knormal e x = runKNormal e (undefined x)

throw :: Info -> Doc -> KNormal ()
throw info mes = throwError (KNormalError info mes)
newTmp :: Name -> Type -> KNormal TypedID
newTmp name typ = do
  c <- gets _count
  modify $ \e -> e { _count = c + 1 }
  return (TypedID (Inserted name c) typ)

insertLet
  :: Expr TypedID
     -> (Expr TypedID
         -> KNormal (Expr TypedID))
     -> KNormal (Expr TypedID)
insertLet v k = do
  x <- newTmp "k" (typeOf v)
  v' <- transExpr v
  e' <- k (Var dummyInfo x)
  return (Let dummyInfo [ValDec dummyInfo x (typeOf v) v'] e')

transExpr :: Expr TypedID -> KNormal (Expr TypedID)
transExpr (Call i0 fn@Var{} args) =
  bind args [] (return . Call i0 fn )
  where bind [] args' k     = k (reverse args')
        bind (x:xs) args' k = insertLet x (\x' -> bind xs (x':args') k)
transExpr (Call{}) = undefined
-- transExpr (T.Call _ _, _) = KNormal $ lift . Left $ "error: function value must be a variable"
-- transExpr (T.BinOp op e1 e2, ty) = do
--   ty' <- transType ty
--   insertLet e1 (\x -> insertLet e2 (\y -> return (BinOp op x y, ty')))
-- transExpr (T.If c t f, ty) =
--   insertLet c (\c' -> do
--                   t' <- transExpr t
--                   f' <- transExpr f
--                   ty' <- transType ty
--                   return (If c' t' f', ty'))
-- transExpr (T.Int x, ty) = (,) <$> pure (Int x) <*> transType ty
-- transExpr (T.Float x, ty) = (,) <$> pure (Float x) <*> transType ty
-- transExpr (T.Bool x, ty) = (,) <$> pure (Bool x) <*> transType ty
-- transExpr (T.Char x, ty) = (,) <$> pure (Char x) <*> transType ty
-- transExpr (T.String x, ty) = (,) <$> pure (String x) <*> transType ty
-- transExpr (T.Unit, ty) = (,) <$> pure Unit <*> transType ty
-- transExpr (T.Let (T.ValDec name typ val) body, ty) = do
--   val' <- transExpr val
--   typ' <- transType typ

--   name' <- newId name -- shadowingのため、先にvalを処理する

--   body' <- transExpr body
--   ty' <- transType ty
--   return (Let (ValDec name' typ' val') body', ty')
-- transExpr (T.Let (T.FunDec fn params retTy fbody) body, ty) = do
--   fn' <- newId fn
--   params' <- mapM (\(n, t) -> (,) <$> newId n <*> transType t) params
--   retTy' <- transType retTy
--   fbody' <- transExpr fbody
--   body' <- transExpr body
--   ty' <- transType ty
--   return (Let (FunDec fn' params' retTy' fbody') body', ty')
-- transExpr (T.Var x, ty) = (,) <$> (Var <$> getId x) <*> transType ty
-- transExpr (T.Seq e1 e2, ty) = do
--   x' <- newId "_"
--   e1' <- transExpr e1
--   e2' <- transExpr e2
--   unitTy <- NameTy <$> rawId "Unit"
--   ty' <- transType ty
--   return (Let (ValDec x' unitTy e1') e2', ty')
