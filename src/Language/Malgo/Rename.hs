{-# LANGUAGE OverloadedStrings #-}

module Language.Malgo.Rename
    ( rename
    , ID(..)
    , RnEnv(..)
    ) where

import           Control.Monad.Error.Class
import           Control.Monad.State.Class
import qualified Data.Map.Strict           as Map
import           Data.Monoid
import           Language.Malgo.Syntax     hiding (info)
import           Language.Malgo.Utils
import           Text.PrettyPrint          (Doc, int, text)
import qualified Text.PrettyPrint          as P

data ID
    = Toplevel { _name :: Name
               , _uniq :: Int }
    | Internal { _name :: Name
               , _uniq :: Int }
    | External { _name :: Name
               , _uniq :: Int }
    deriving (Show, Ord, Read)

instance Eq ID where
    x == y = _uniq x == _uniq y

instance PrettyPrint ID where
    pretty (Toplevel name _) = pretty name
    pretty (Internal name u) = pretty name <> text "." <> int u
    pretty (External name _) = pretty name

data RnEnv = RnEnv
    { knowns :: Map.Map Name ID
    , idCons :: Name -> Int -> ID
    , _gen   :: Int
    }

instance Env RnEnv where
    initEnv = RnEnv Map.empty Toplevel
    updateUniq e i = e { _gen = i }
    getUniq = _gen

type Rename m a = MalgoT RnEnv m a

rename :: Monad m => Expr Name -> Rename m (Expr ID)
rename = transExpr

throw :: Monad m => Info -> Doc -> Rename m a
throw info mes = throwError (RenameError info mes)

newID :: Monad m => Name -> Rename m ID
newID orig = do
    c <- newUniq
    cons <- gets idCons
    let i = cons orig c
    modify $ \e -> e {knowns = Map.insert orig i (knowns e)}
    return i

getID :: Monad m => Info -> Name -> Rename m ID
getID info name = do
    k <- gets knowns
    case Map.lookup name k of
        Just x  -> return x
        Nothing -> throw info (pretty name P.<+> text "is not defined")

transExpr :: Monad m => Expr Name -> Rename m (Expr ID)
transExpr (Var info name) = Var info <$> getID info name
transExpr (Int info x) = return $ Int info x
transExpr (Float info x) = return $ Float info x
transExpr (Bool info x) = return $ Bool info x
transExpr (Char info x) = return $ Char info x
transExpr (String info x) = return $ String info x
transExpr (Unit info) = return $ Unit info
transExpr (Tuple info xs) = Tuple info <$> mapM transExpr xs
transExpr (TupleAccess info e i) = TupleAccess info <$> transExpr e <*> pure i
transExpr (Fn info params body) = do
  backup <- get
  modify $ \e -> e {idCons = Internal}
  params' <- mapM (\(n, t) -> (,) <$> newID n <*> pure t) params
  body' <- transExpr body
  put backup
  return (Fn info params' body')
transExpr (Call info fn args) =
    Call info <$> transExpr fn <*> mapM transExpr args
transExpr (Seq info e1 e2) = Seq info <$> transExpr e1 <*> transExpr e2
transExpr (Let info decls e) = do
    cons <- gets idCons
    decls' <- mapM transDecl decls
    modify $ \env -> env {idCons = Internal}
    e' <- transExpr e
    modify $ \env -> env {idCons = cons}
    return (Let info decls' e')
transExpr (If info c t f) =
    If info <$> transExpr c <*> transExpr t <*> transExpr f
transExpr (BinOp info op x y) = BinOp info op <$> transExpr x <*> transExpr y

transDecl :: Monad m => Decl Name -> Rename m (Decl ID)
transDecl (ValDec info name typ val) = do
    backup <- get
    modify $ \e -> e {idCons = Internal}
    val' <- transExpr val
    put backup
    name' <- newID name
    return (ValDec info name' typ val')
transDecl (FunDec info fn params retty body) = do
    fn' <- newID fn
    backup <- get
    modify $ \e -> e {idCons = Internal}
    params' <- mapM (\(n, t) -> (,) <$> newID n <*> pure t) params
    body' <- transExpr body
    put backup
    return (FunDec info fn' params' retty body')
transDecl (ExDec info name typ orig) = do
    cons <- gets idCons
    modify $ \e -> e {idCons = External}
    name' <- newID name
    modify $ \e -> e {idCons = cons}
    return $ ExDec info name' typ orig
