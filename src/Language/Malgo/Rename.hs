{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.Malgo.Rename ( rename ) where

import           Control.Lens           (makeLenses)
import qualified Data.Map.Strict        as Map

import           Language.Malgo.ID
import           Language.Malgo.Monad
import           Language.Malgo.Prelude
import           Language.Malgo.IR.Syntax  hiding (info)

data RnEnv = RnEnv { _knowns     :: Map.Map Name RawID
                   , _uniqSupply :: UniqSupply
                   }
makeLenses ''RnEnv

instance MalgoEnv RnEnv where
  uniqSupplyL = uniqSupply
  genEnv us = RnEnv mempty us

rename :: MonadMalgo RnEnv m => Expr Name -> m (Expr RawID)
rename x = transExpr x

addKnowns :: MonadMalgo RnEnv m => [(Name, RawID)] -> m a -> m a
addKnowns kvs m =
  addTable kvs knowns m

getID :: MonadMalgo RnEnv m => Info -> Name -> m RawID
getID info name =
  lookupTable ("error(rename):" <+> pretty info <+> pretty name <+> "is not defined") name knowns

transExpr :: MonadMalgo RnEnv m => Expr Name -> m (Expr RawID)
transExpr (Var info name) = Var info <$> getID info name
transExpr (Int info x) = pure $ Int info x
transExpr (Float info x) = pure $ Float info x
transExpr (Bool info x) = pure $ Bool info x
transExpr (Char info x) = pure $ Char info x
transExpr (String info x) = pure $ String info x
transExpr (Unit info) = pure $ Unit info
transExpr (Tuple info xs) = Tuple info <$> mapM transExpr xs
transExpr (TupleAccess info e i) = TupleAccess info <$> transExpr e <*> pure i
transExpr (Fn info params body) = do
  paramIDs <- mapM (flip newID () . fst) params
  addKnowns (zip (map fst params) paramIDs) $ do
    params' <- mapM (\(n, t) -> (,) <$> getID info n <*> pure t) params
    body' <- transExpr body
    pure (Fn info params' body')
transExpr (Call info fn args) =
  Call info <$> transExpr fn <*> mapM transExpr args
transExpr (Seq info e1 e2) = Seq info <$> transExpr e1 <*> transExpr e2
transExpr (Let info decls e) = do
  declIDs <- mapM (flip newID () . getName) decls
  addKnowns (zip (map getName decls) declIDs) $ do
    decls' <- mapM transDecl decls
    e' <- transExpr e
    pure (Let info decls' e')
  where getName (ExDec _ name _ _)    = name
        getName (FunDec _ name _ _ _) = name
        getName (ValDec _ name _ _)   = name
transExpr (If info c t f) =
  If info <$> transExpr c <*> transExpr t <*> transExpr f
transExpr (BinOp info op x y) = BinOp info op <$> transExpr x <*> transExpr y

transDecl :: MonadMalgo RnEnv m => Decl Name -> m (Decl RawID)
transDecl (ValDec info name typ val) = do
  val' <- transExpr val
  name' <- getID info name
  pure (ValDec info name' typ val')
transDecl (FunDec info fn params retty body) = do
  fn' <- getID info fn
  paramIDs <- mapM (flip newID () . fst) params
  addKnowns (zip (map fst params) paramIDs) $ do
    params' <- mapM (\(n, t) -> (,) <$> getID info n <*> pure t) params
    body' <- transExpr body
    pure (FunDec info fn' params' retty body')
transDecl (ExDec info name typ orig) = do
  name' <- getID info name
  pure $ ExDec info name' typ orig
