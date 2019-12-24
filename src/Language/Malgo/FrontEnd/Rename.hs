{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.FrontEnd.Rename
  ( Rename
  )
where

import           Data.List                      ( span )
import qualified Data.Map.Strict               as Map
import           Language.Malgo.FrontEnd.Info
import           Language.Malgo.ID
import           Language.Malgo.IR.Syntax
                                         hiding ( info )
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Pretty
import           Relude

data Rename

instance Pass Rename (Expr Text) (Expr (ID ())) where
  isDump = dumpRenamed
  trans s = runReaderT (renameExpr s) mempty

type RenameM a = ReaderT (Map Text (ID ())) MalgoM a

withKnowns :: [(Text, ID ())] -> RenameM a -> RenameM a
withKnowns kvs = local (Map.fromList kvs <>)

getID :: Info -> Text -> RenameM (ID ())
getID info name = do
  k <- ask
  case Map.lookup name k of
    Just x -> return x
    Nothing ->
      errorDoc
        $   "error(rename):"
        <+> pPrint info
        <+> pPrint name
        <+> "is not defined"

renameExpr :: Expr Text -> RenameM (Expr (ID ()))
renameExpr (Var    info name) = Var info <$> getID info name
renameExpr (Int    info x   ) = return $ Int info x
renameExpr (Float  info x   ) = return $ Float info x
renameExpr (Bool   info x   ) = return $ Bool info x
renameExpr (Char   info x   ) = return $ Char info x
renameExpr (String info x   ) = return $ String info x
renameExpr (Unit info       ) = return $ Unit info
renameExpr (Tuple info xs   ) = Tuple info <$> mapM renameExpr xs
renameExpr (TupleAccess info e i) =
  TupleAccess info <$> renameExpr e <*> pure i
renameExpr (MakeArray info ty size) = MakeArray info ty <$> renameExpr size
renameExpr (ArrayRead info arr ix) =
  ArrayRead info <$> renameExpr arr <*> renameExpr ix
renameExpr (ArrayWrite info arr ix val) =
  ArrayWrite info <$> renameExpr arr <*> renameExpr ix <*> renameExpr val
renameExpr (Fn info params body) = do
  paramIDs <- mapM (newID () . fst) params
  withKnowns (zip (map fst params) paramIDs) $ do
    params' <- mapM (\(n, t) -> (,) <$> getID info n <*> pure t) params
    body'   <- renameExpr body
    return $ Fn info params' body'
renameExpr (Call info fn args) =
  Call info <$> renameExpr fn <*> mapM renameExpr args
renameExpr (Seq info e1    e2) = Seq info <$> renameExpr e1 <*> renameExpr e2
renameExpr (Let info decls e ) = do
  let splitedDecls = splitDecl decls
  k <- renameDecls info splitedDecls
  k e
 where
  splitDecl [] = []
  splitDecl ds@(FunDec{} : _) =
    let (fundecs, rest) = span isFunDec ds in Fun fundecs : splitDecl rest
  splitDecl (d@ValDec{} : ds) = Val d : splitDecl ds
  splitDecl (d@ExDec{}  : ds) = Ex d : splitDecl ds
  isFunDec FunDec{} = True
  isFunDec _        = False
renameExpr (If info c t f) =
  If info <$> renameExpr c <*> renameExpr t <*> renameExpr f
renameExpr (BinOp info op x y) =
  BinOp info op <$> renameExpr x <*> renameExpr y

data SplitedDecl = Val (Decl Text)
                 | Fun [Decl Text]
                 | Ex (Decl Text)

renameDecls
  :: Info -> [SplitedDecl] -> RenameM (Expr Text -> RenameM (Expr (ID ())))
renameDecls _     [] = pure $ \e -> renameExpr e
renameDecls info0 (Val (ValDec info1 name typ val) : ds) = do
  val'  <- renameExpr val
  name' <- newID () name
  k     <- withKnowns [(name, name')] $ renameDecls info0 ds
  pure $ \e -> Let info0 [ValDec info1 name' typ val']
    <$> withKnowns [(name, name')] (k e)
renameDecls info0 (Fun fs : ds) = do
  functionNames <- mapM (newID () . getName) fs
  let newKnowns = zip (map getName fs) functionNames
  fs' <- withKnowns newKnowns $ mapM renameFunDec fs
  k   <- withKnowns newKnowns $ renameDecls info0 ds
  pure $ \e -> Let info0 fs' <$> withKnowns newKnowns (k e)
 where
  getName (FunDec _ name _ _ _) = name
  getName _                     = error "unreachable(renameDecls#getName)"
renameDecls info0 (Ex (ExDec info1 name typ orig) : ds) = do
  name' <- newID () name
  k     <- withKnowns [(name, name')] $ renameDecls info0 ds
  pure $ \e -> Let info1 [ExDec info1 name' typ orig]
    <$> withKnowns [(name, name')] (k e)
renameDecls _ _ = error "unreachable(renameDecls)"

renameFunDec :: Decl Text -> RenameM (Decl (ID ()))
renameFunDec (FunDec info fn params retty body) = do
  fn'      <- getID info fn
  paramIDs <- mapM (newID () . fst) params
  withKnowns (zip (map fst params) paramIDs) $ do
    params' <- mapM (\(n, t) -> (, t) <$> getID info n) params
    body'   <- renameExpr body
    return (FunDec info fn' params' retty body')
renameFunDec _ = error "unreachable(renameFunDec)"
