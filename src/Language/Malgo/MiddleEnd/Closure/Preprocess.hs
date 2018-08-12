{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}
module Language.Malgo.MiddleEnd.Closure.Preprocess (divideTypeFromExpr) where

import qualified Data.Map.Strict      as Map
import           Language.Malgo.ID
import           Language.Malgo.IR.IR
import           Universum

divideTypeFromExpr :: Expr (ID MType) -> (Expr (ID ()), Map (ID ()) MType)
divideTypeFromExpr e = runState (divide e) mempty

divide :: Monad m => Expr (ID MType) -> StateT (Map (ID ()) MType) m (Expr (ID ()))
divide (Var a) = return $ Var (removeType a)
divide (Int x) = return $ Int x
divide (Float x) = return $ Float x
divide (Bool x) = return $ Bool x
divide (Char x) = return $ Char x
divide (String x) = return $ String x
divide Unit = return Unit
divide (Prim orig ty) = return $ Prim orig ty
divide (Tuple xs) = return $ Tuple (map removeType xs)
divide (Apply fn args) = return $ Apply (removeType fn) (map removeType args)
divide (Let var val e) = do
  addTypeTable var
  Let (removeType var) <$> divide val <*> divide e
divide (LetRec decs body) =
  LetRec <$> mapM divide' decs <*> divide body
  where divide' (fn, params, fbody) = do
          addTypeTable fn
          mapM_ addTypeTable params
          fbody' <- divide fbody
          return (removeType fn, map removeType params, fbody')
divide (Cast ty a) = return $ Cast ty (removeType a)
divide (Access a is) = return $ Access (removeType a) is
divide (If c t f) = If (removeType c) <$> divide t <*> divide f

removeType :: ID MType -> ID ()
removeType (ID name uniq _) = ID name uniq ()

addTypeTable :: Monad m => ID MType -> StateT (Map (ID ()) MType) m ()
addTypeTable i =
  modify (Map.insert (removeType i) (view idMeta i))
