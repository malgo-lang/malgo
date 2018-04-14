{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Malgo.Rename ( rename ) where

import qualified Data.Map.Strict        as Map
import           Language.Malgo.ID
import           Language.Malgo.Monad
import           Language.Malgo.Prelude
import           Language.Malgo.Syntax  hiding (info)
import           Language.Malgo.Utils
import qualified Text.PrettyPrint       as P

data RnEnv = RnEnv { knowns      :: Map.Map Name ID
                   , _uniqSupply :: Int
                   }
  deriving Generic

instance Default RnEnv

instance HasUniqSupply RnEnv where
  uniqSupply = lens _uniqSupply (\s i -> s { _uniqSupply = i })

type Rename a = Malgo RnEnv a

rename :: Expr Name -> Rename (Expr ID)
rename = transExpr

throw :: Info -> P.Doc -> Rename a
throw info mes = malgoError $ "error(rename):" P.<+> pretty info P.<+> mes

newID :: Name -> Rename ID
newID orig = do
  c <- newUniq
  let i = ID orig c
  modify $ \e -> e {knowns = Map.insert orig i (knowns e)}
  pure i

getID :: Info -> Name -> Rename ID
getID info name = do
  k <- gets knowns
  case lookup name k of
    Just x  -> pure x
    Nothing -> throw info (pretty name P.<+> P.text "is not defined")

transExpr :: Expr Name -> Rename (Expr ID)
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
  params' <- mapM (\(n, t) -> (,) <$> newID n <*> pure t) params
  body' <- transExpr body
  pure (Fn info params' body')
transExpr (Call info fn args) =
  Call info <$> transExpr fn <*> mapM transExpr args
transExpr (Seq info e1 e2) = Seq info <$> transExpr e1 <*> transExpr e2
transExpr (Let info decls e) = do
  decls' <- mapM transDecl decls
  e' <- transExpr e
  pure (Let info decls' e')
transExpr (If info c t f) =
  If info <$> transExpr c <*> transExpr t <*> transExpr f
transExpr (BinOp info op x y) = BinOp info op <$> transExpr x <*> transExpr y

transDecl :: Decl Name -> Rename (Decl ID)
transDecl (ValDec info name typ val) = do
  val' <- transExpr val
  name' <- newID name
  pure (ValDec info name' typ val')
transDecl (FunDec info fn params retty body) = do
  fn' <- newID fn
  ((params', body'), _) <- sandbox $ do
    params' <- mapM (\(n, t) -> (,) <$> newID n <*> pure t) params
    body' <- transExpr body
    pure (params', body')
  pure (FunDec info fn' params' retty body')
transDecl (ExDec info name typ orig) = do
  name' <- newID name
  pure $ ExDec info name' typ orig
