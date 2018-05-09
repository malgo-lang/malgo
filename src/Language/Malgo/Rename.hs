{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Language.Malgo.Rename ( rename ) where

import           Control.Lens           (at, makeLensesFor, use, view, (?=))
import qualified Data.Map.Strict        as Map
import qualified Text.PrettyPrint       as P

import           Language.Malgo.ID
import           Language.Malgo.Monad
import           Language.Malgo.Prelude
import           Language.Malgo.Syntax  hiding (info)

data RnEnv = RnEnv { _knowns     :: Map.Map Name ID
                   , _uniqSupply :: UniqSupply
                   }
  deriving (Generic, Default, HasUniqSupply)

makeLensesFor [("_knowns", "knowns")] ''RnEnv

type Rename a = Malgo RnEnv a

rename :: Expr Name -> Rename (Expr ID)
rename = transExpr

newID :: Name -> Rename ID
newID orig = do
  c <- newUniq
  let i = ID orig c
  (knowns . at orig) ?= i
  pure i

getID :: Info -> Name -> Rename ID
getID info name = do
  k <- use knowns
  case view (at name) k of
    Just x  -> pure x
    Nothing -> malgoError $ "error(rename):" P.<+> ppr info P.<+> ppr name P.<+> P.text "is not defined"

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
  mapM_ (newID . getName) decls
  decls' <- mapM transDecl decls
  e' <- transExpr e
  pure (Let info decls' e')
  where getName (ExDec _ name _ _)    = name
        getName (FunDec _ name _ _ _) = name
        getName (ValDec _ name _ _)   = name
transExpr (If info c t f) =
  If info <$> transExpr c <*> transExpr t <*> transExpr f
transExpr (BinOp info op x y) = BinOp info op <$> transExpr x <*> transExpr y

transDecl :: Decl Name -> Rename (Decl ID)
transDecl (ValDec info name typ val) = do
  val' <- transExpr val
  name' <- getID info name
  pure (ValDec info name' typ val')
transDecl (FunDec info fn params retty body) = do
  fn' <- getID info fn
  (params', body') <- sandbox $ do
    params' <- mapM (\(n, t) -> (,) <$> newID n <*> pure t) params
    body' <- transExpr body
    pure (params', body')
  pure (FunDec info fn' params' retty body')
transDecl (ExDec info name typ orig) = do
  name' <- getID info name
  pure $ ExDec info name' typ orig
