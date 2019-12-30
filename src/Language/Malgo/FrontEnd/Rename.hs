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

instance Pass Rename (Expr Text) (Expr (ID ())) where
  isDump = dumpRenamed
  trans s = runReaderT (renameExpr s) mempty

type RenameM a = ReaderT (Map Text (ID ())) MalgoM a

withKnowns :: (MonadMalgo m, MonadReader (Map Text (ID ())) m) => [Text] -> m b -> m b
withKnowns ks m = do
  vs <- mapM (newID ()) ks
  local (fromList (zip ks vs) <>) m

getID :: Info -> Text -> RenameM (ID ())
getID info name = do
  k <- ask
  case lookup name k of
    Just x  -> return x
    Nothing -> errorDoc $ "error(rename):" <+> pPrint info <+> pPrint name <+> "is not defined"

renameExpr :: Expr Text -> RenameM (Expr (ID ()))
renameExpr (Var    info name          ) = Var info <$> getID info name
renameExpr (Int    info x             ) = return $ Int info x
renameExpr (Float  info x             ) = return $ Float info x
renameExpr (Bool   info x             ) = return $ Bool info x
renameExpr (Char   info x             ) = return $ Char info x
renameExpr (String info x             ) = return $ String info x
renameExpr (Unit info                 ) = return $ Unit info
renameExpr (Tuple info xs             ) = Tuple info <$> mapM renameExpr xs
renameExpr (TupleAccess info e    i   ) = TupleAccess info <$> renameExpr e <*> pure i
renameExpr (MakeArray   info init size) = MakeArray info <$> renameExpr init <*> renameExpr size
renameExpr (ArrayRead   info arr  ix  ) = ArrayRead info <$> renameExpr arr <*> renameExpr ix
renameExpr (ArrayWrite info arr ix val) =
  ArrayWrite info <$> renameExpr arr <*> renameExpr ix <*> renameExpr val
renameExpr (Fn info params body) = withKnowns (map fst params) $ do
  params' <- mapM (\(n, t) -> (, t) <$> getID info n) params
  body'   <- renameExpr body
  return $ Fn info params' body'
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
      params' <- mapM (\(p, t) -> (, t) <$> getID info p) params
      body'   <- renameExpr body
      return (info, fn', params', retty, body')
renameExpr (Let info0 (ExDec info1 name typ orig) e) =
  withKnowns [name]
    $   Let info0
    <$> (ExDec info1 <$> (getID info1 name) <*> pure typ <*> pure orig)
    <*> renameExpr e
renameExpr (If    info c  t f) = If info <$> renameExpr c <*> renameExpr t <*> renameExpr f
renameExpr (BinOp info op x y) = BinOp info op <$> renameExpr x <*> renameExpr y
