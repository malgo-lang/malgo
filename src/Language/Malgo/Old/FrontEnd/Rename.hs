{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Malgo.Old.FrontEnd.Rename ( rename ) where

import qualified Data.Map.Strict              as Map
import           Language.Malgo.Old.FrontEnd.Info
import           Language.Malgo.Old.ID
import           Language.Malgo.Old.IR.Syntax     hiding (info)
import           Language.Malgo.Old.Monad
import           Language.Malgo.Pretty
import           Universum

rename :: Expr Text -> MalgoM (Expr RawID)
rename e =
  runReaderT (renameExpr e) Map.empty

type RenameM a = ReaderT (Map Text RawID) MalgoM a

withKnowns :: [(Text, RawID)] -> RenameM a -> RenameM a
withKnowns kvs =
  local (Map.fromList kvs <>)

getID :: Info -> Text -> RenameM RawID
getID info name = do
  k <- ask
  case Map.lookup name k of
    Just x -> return x
    Nothing -> malgoError $ "error(rename):" <+> pPrint info <+> pPrint name <+> "is not defined"

renameExpr :: Expr Text -> RenameM (Expr RawID)
renameExpr (Var info name) = Var info <$> getID info name
renameExpr (Int info x) = return $ Int info x
renameExpr (Float info x) = return $ Float info x
renameExpr (Bool info x) = return $ Bool info x
renameExpr (Char info x) = return $ Char info x
renameExpr (String info x) = return $ String info x
renameExpr (Unit info) = return $ Unit info
renameExpr (Tuple info xs) = Tuple info <$> mapM renameExpr xs
renameExpr (TupleAccess info e i) =
  TupleAccess info <$> renameExpr e <*> pure i
renameExpr (MakeArray info ty size) =
  MakeArray info ty <$> renameExpr size
renameExpr (ArrayRead info arr ix) =
  ArrayRead info <$> renameExpr arr <*> renameExpr ix
renameExpr (ArrayWrite info arr ix val) =
  ArrayWrite info <$> renameExpr arr <*> renameExpr ix <*> renameExpr val
renameExpr (Fn info params body) = do
  paramIDs <- mapM (newID () . fst) params
  withKnowns (zip (map fst params) paramIDs) $ do
    params' <- mapM (\(n, t) -> (,) <$> getID info n <*> pure t) params
    body' <- renameExpr body
    return $ Fn info params' body'
renameExpr (Call info fn args) =
  Call info <$> renameExpr fn <*> mapM renameExpr args
renameExpr (Seq info e1 e2) =
  Seq info <$> renameExpr e1 <*> renameExpr e2
renameExpr (Let info decls e) = do
  declIDs <- mapM (newID () . getName) decls
  withKnowns (zip (map getName decls) declIDs) $ do
    decls' <- mapM renameDecl decls
    e' <- renameExpr e
    pure (Let info decls' e')
  where getName (ExDec _ name _ _)    = name
        getName (FunDec _ name _ _ _) = name
        getName (ValDec _ name _ _)   = name
renameExpr (If info c t f) =
  If info <$> renameExpr c <*> renameExpr t <*> renameExpr f
renameExpr (BinOp info op x y) = BinOp info op <$> renameExpr x <*> renameExpr y

renameDecl :: Decl Text -> RenameM (Decl RawID)
renameDecl (ValDec info name typ val) = do
  val' <- renameExpr val
  name' <- getID info name
  return (ValDec info name' typ val')
renameDecl (FunDec info fn params retty body) = do
  fn' <- getID info fn
  paramIDs <- mapM (newID () . fst) params
  withKnowns (zip (map fst params) paramIDs) $ do
    params' <- mapM (\(n, t) -> (,) <$> getID info n <*> pure t) params
    body' <- renameExpr body
    return (FunDec info fn' params' retty body')
renameDecl (ExDec info name typ orig) = do
  name' <- getID info name
  return $ ExDec info name' typ orig
