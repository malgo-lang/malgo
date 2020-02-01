{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TupleSections         #-}
module Language.Malgo.FrontEnd.Rename
  ( Rename
  )
where

import           Language.Malgo.FrontEnd.Info
import           Language.Malgo.ID
import           Language.Malgo.IR.Syntax
                                         hiding ( info )
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Pretty
import           Language.Malgo.Prelude

data Rename

instance Pass Rename (Expr String) (Expr (ID ())) where
  passName = "Rename"
  isDump   = dumpRenamed
  trans s = runReaderT (renameExpr s) mempty

type Known = Map String (ID ())

withKnowns :: (MonadUniq m, MonadReader Known m) => [String] -> m b -> m b
withKnowns ks m = do
  vs <- mapM (newID ()) ks
  local (fromList (zip ks vs) <>) m

getID :: MonadReader Known m => Info -> String -> m (ID ())
getID info name = do
  k <- ask
  case lookup name k of
    Just x  -> pure x
    Nothing -> errorDoc $ "error(rename):" <+> pPrint info <+> pPrint name <+> "is not defined"

renameExpr :: (MonadUniq m, MonadReader Known m) => Expr String -> m (Expr (ID ()))
renameExpr (Var    info name          ) = Var info <$> getID info name
renameExpr (Int    info x             ) = pure $ Int info x
renameExpr (Float  info x             ) = pure $ Float info x
renameExpr (Bool   info x             ) = pure $ Bool info x
renameExpr (Char   info x             ) = pure $ Char info x
renameExpr (String info x             ) = pure $ String info x
renameExpr (Unit info                 ) = pure $ Unit info
renameExpr (Tuple info xs             ) = Tuple info <$> mapM renameExpr xs
renameExpr (TupleAccess info e    i   ) = TupleAccess info <$> renameExpr e <*> pure i
renameExpr (MakeArray   info init size) = MakeArray info <$> renameExpr init <*> renameExpr size
renameExpr (ArrayRead   info arr  ix  ) = ArrayRead info <$> renameExpr arr <*> renameExpr ix
renameExpr (ArrayWrite info arr ix val) =
  ArrayWrite info <$> renameExpr arr <*> renameExpr ix <*> renameExpr val
renameExpr (Fn info params body) = withKnowns (map fst params) $ do
  params' <- mapM (ltraverse (getID info)) params
  body'   <- renameExpr body
  pure $ Fn info params' body'
renameExpr (Call info fn args) = Call info <$> renameExpr fn <*> mapM renameExpr args
renameExpr (Seq info e1 e2) = Seq info <$> renameExpr e1 <*> renameExpr e2
renameExpr (Let info0 (ValDec info1 name typ val) e) = do
  val' <- renameExpr val
  withKnowns [name]
    $   Let info0
    <$> (ValDec info1 <$> getID info1 name <*> pure typ <*> pure val')
    <*> renameExpr e
renameExpr (Let info0 (FunDec fs) e) = withKnowns (map getName fs) $ do
  fs' <- mapM renameFunDec fs
  Let info0 (FunDec fs') <$> renameExpr e
 where
  getName (_, f, _, _, _) = f
  renameFunDec (info, fn, params, retty, body) = do
    fn' <- getID info fn
    withKnowns (map fst params) $ do
      params' <- mapM (ltraverse (getID info)) params
      body'   <- renameExpr body
      pure (info, fn', params', retty, body')
renameExpr (Let info0 (ExDec info1 name typ orig) e) =
  withKnowns [name]
    $   Let info0
    <$> (ExDec info1 <$> getID info1 name <*> pure typ <*> pure orig)
    <*> renameExpr e
renameExpr (If    info c  t f) = If info <$> renameExpr c <*> renameExpr t <*> renameExpr f
renameExpr (BinOp info op x y) = BinOp info op <$> renameExpr x <*> renameExpr y
renameExpr (Match info scrutinee clauses) =
  Match info <$> renameExpr scrutinee <*> mapM (renameClause info) clauses

renameClause :: (MonadUniq m, MonadReader Known m)
             => Info
             -> (Pat String, Expr String)
             -> m (Pat (ID ()), Expr (ID ()))
renameClause info (p, e) = renamePat info p $ \p' -> (p', ) <$> renameExpr e

renamePat :: (MonadUniq m, MonadReader Known m) => Info -> Pat String -> (Pat (ID ()) -> m b) -> m b
renamePat info (VarP   x ) k = withKnowns [x] $ VarP <$> getID info x >>= k
renamePat info (TupleP ps) k = go ps []
 where
  go []       acc = k (TupleP $ reverse acc)
  go (x : xs) acc = renamePat info x $ \x' -> go xs (x' : acc)
