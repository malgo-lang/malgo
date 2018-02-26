{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Malgo.Rename
  ( rename
  , ID(..)
  , RnEnv(..)
  ) where

import qualified Data.Map.Strict        as Map
import           Language.Malgo.Prelude
import           Language.Malgo.Syntax  hiding (info)
import           Language.Malgo.Utils
import qualified Text.PrettyPrint       as P

data ID = ID { _name :: Name, _uniq :: Int }
  deriving (Show, Ord, Read)

instance Eq ID where
  x == y = _uniq x == _uniq y

instance PrettyPrint ID where
  pretty (ID name u) = pretty name <> P.text "." <> P.int u

newtype RnEnv = RnEnv { knowns :: Map.Map Name ID }

instance Env RnEnv where
  initEnv = RnEnv Map.empty

type Rename m a = MalgoT RnEnv m a

rename :: Monad m => Expr Name -> Rename m (Expr ID)
rename = transExpr

throw :: Monad m => Info -> P.Doc -> Rename m a
throw info mes = throwError (RenameError info mes)

newID :: Monad m => Name -> Rename m ID
newID orig = do
  c <- newUniq
  -- cons <- gets idCons
  let i = ID orig c
  modify $ \e -> e {knowns = Map.insert orig i (knowns e)}
  pure i

getID :: Monad m => Info -> Name -> Rename m ID
getID info name = do
  k <- gets knowns
  case lookup name k of
    Just x  -> pure x
    Nothing -> throw info (pretty name P.<+> P.text "is not defined")

transExpr :: Monad m => Expr Name -> Rename m (Expr ID)
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

transDecl :: Monad m => Decl Name -> Rename m (Decl ID)
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
