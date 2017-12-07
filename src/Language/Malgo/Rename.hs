{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.Rename (rename, ID(..), RnEnv(..)) where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map.Strict       as Map
import           Data.Monoid
import           Language.Malgo.Syntax
import           Language.Malgo.Utils
import           Text.PrettyPrint      (Doc, int, text)
import qualified Text.PrettyPrint      as P

data ID = Toplevel { _name :: Name
                   , _uniq :: Int
                   }
        | Internal { _name :: Name
                   , _uniq :: Int
                   }
        | External { _name :: Name
                   , _uniq :: Int
                   }
        | Inserted { _name :: Name
                   , _uniq :: Int
                   }
        deriving (Show, Ord, Eq)

instance PrettyPrint ID where
  pretty (Toplevel name u) = pretty name <> P.braces (int u)
  pretty (Internal name u) = pretty name <> text "." <> int u
  pretty (External name u) = pretty name <> P.braces (int u)
  pretty (Inserted name u) = text "$" <> pretty name <> text "." <> int u

data RnEnv = RnEnv { count  :: Int
                   , knowns :: Map.Map Name ID
                   , idCons :: Name -> Int -> ID
                   }

initRnEnv :: RnEnv
initRnEnv = RnEnv
            { count = 0
            , knowns = Map.empty
            , idCons = Toplevel
            }

type Rename a = Malgo RnEnv a

runRename :: Malgo RnEnv a -> (Either MalgoError a, RnEnv)
runRename m = runMalgo m initRnEnv

rename :: Expr Name -> (Either MalgoError (Expr ID), RnEnv)
rename x = runRename (transExpr x)

throw :: Info -> Doc -> Rename a
throw info mes = throwError (RenameError info mes)

newID :: Name -> Rename ID
newID orig = do
  c <- gets count
  cons <- gets idCons
  let i = cons orig c
  modify $ \e -> e { count = count e + 1
                   , knowns = Map.insert orig i (knowns e)
                   }
  return i

getID :: Info -> Name -> Rename ID
getID info name = do
  k <- gets knowns
  case Map.lookup name k of
    Just x  -> return x
    Nothing -> throw info (pretty name P.<+> text "is not defined")

transExpr :: Expr Name -> Rename (Expr ID)
transExpr (Var info name) = Var info <$> getID info name
transExpr (Int info x)    = return $ Int info x
transExpr (Float info x)  = return $ Float info x
transExpr (Bool info x)   = return $ Bool info x
transExpr (Char info x)   = return $ Char info x
transExpr (String info x) = return $ String info x
transExpr (Unit info)     = return $ Unit info
transExpr (Call info fn args) = Call info <$> transExpr fn <*> mapM transExpr args
transExpr (Seq info e1 e2) = Seq info <$> transExpr e1 <*> transExpr e2
transExpr (Let info decls e) = do
  cons <- gets idCons

  decls' <- mapM transDecl decls
  modify $ \e -> e { idCons = Internal }
  e' <- transExpr e
  modify $ \e -> e { idCons = cons }

  return (Let info decls' e')
transExpr (If info c t f) = If info <$> transExpr c <*> transExpr t <*> transExpr f
transExpr (BinOp info op x y) = BinOp info op <$> transExpr x <*> transExpr y

transDecl :: Decl Name -> Rename (Decl ID)
transDecl (ValDec info name typ val) = do
  backup <- get

  modify $ \e -> e { idCons = Internal }
  val' <- transExpr val
  put backup

  name' <- newID name
  return (ValDec info name' typ val')
transDecl (FunDec info fn params retty body) = do
  fn' <- newID fn

  backup <- get
  modify $ \e -> e { idCons = Internal }
  params' <- mapM (\(n, t) -> (,) <$> newID n <*> pure t) params
  body' <- transExpr body
  put backup

  return (FunDec info fn' params' retty body')
transDecl (ExDec info name typ orig) = do
  cons <- gets idCons
  modify $ \e -> e { idCons = External }
  name' <- newID name
  modify $ \e -> e { idCons = cons }
  return $ ExDec info name' typ orig
