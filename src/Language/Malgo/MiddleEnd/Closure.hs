{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.Malgo.MiddleEnd.Closure where

import           Language.Malgo.ID
import           Language.Malgo.IR.IR
import           Language.Malgo.Monad
import           Language.Malgo.Prelude

data Env = Env { _varmap     :: Map (ID MType) (ID MType)
               , _captures   :: Maybe [ID MType]
               , _program    :: IORef (Program (ID MType))
               , _knowns     :: [ID MType]
               , _uniqSupply :: UniqSupply
               }

makeLenses ''Env

instance MalgoEnv Env where
  uniqSupplyL = uniqSupply
  genEnv u = Env mempty Nothing <$> newIORef (Program []) <*> pure [] <*> pure u

addDefn :: MonadMalgo Env m => Defn (ID MType) -> m ()
addDefn defn = do
  p <- access program
  modifyIORef p (\(Program xs) -> Program $ defn:xs)

trans :: Expr (ID MType) -> Malgo Env (Program (ID MType))
trans e = do
  u <- newUniq
  let mainFun = ID "main" u (FunctionTy (IntTy 32) [])
  e' <- transExpr e
  addDefn (DefFun mainFun [] e')
  readIORef =<< access program

updateID :: ID MType -> Malgo Env (ID MType)
updateID a = do
  vm <- access varmap
  return $ fromMaybe a (view (at a) vm)

transExpr :: Expr (ID MType) -> Malgo Env (Expr (ID MType))
transExpr (Var a)    = Var <$> updateID a
transExpr (Tuple xs) = Tuple <$> mapM updateID xs
transExpr (Apply f args) = do
  f' <- updateID f
  k <- access knowns
  if f' `elem` k
    then Apply f' <$> mapM updateID args
    else undefined
transExpr (Let n val body) = do
  val' <- transExpr val
  let n' = n & idMeta .~ mTypeOf val'
  addTable [(n, n')] varmap $
    Let n' val' <$> transExpr body
