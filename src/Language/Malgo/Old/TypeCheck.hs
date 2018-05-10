{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Language.Malgo.Old.TypeCheck
    ( typeCheck
    ) where

import           Control.Lens           (at, view, use, (?=), makeLensesFor)
import           Text.PrettyPrint


import           Language.Malgo.Old.ID
import           Language.Malgo.Old.Monad
import           Language.Malgo.Old.Prelude
import           Language.Malgo.Old.Syntax  hiding (info)
import qualified Language.Malgo.Old.Syntax  as Syntax
import           Language.Malgo.Old.Type
import           Language.Malgo.Old.TypedID

data TcEnv = TcEnv { _table      :: Map ID TypedID
                   , _uniqSupply :: UniqSupply
                   }
  deriving (Generic, Default, HasUniqSupply)

makeLensesFor [("_table", "table")] ''TcEnv

type TypeCheck a = Malgo TcEnv a

typeCheck :: Expr ID -> TypeCheck (Expr TypedID)
typeCheck = checkExpr

throw :: Info -> Doc -> TypeCheck a
throw info mes = malgoError $ "error(typecheck):" <+> ppr info <+> mes

addBind :: ID -> Type -> TypeCheck ()
addBind name typ = (table . at name) ?= TypedID name typ

getBind :: Info -> ID -> TypeCheck TypedID
getBind info name = do
    t <- use table
    maybe (throw info (ppr name <+> "is not defined")) return (view (at name) t)

prototypes :: [Decl a] -> [(a, Type)]
prototypes xs = map mkPrototype (filter hasPrototype xs)
  where hasPrototype ExDec{}  = True
        hasPrototype FunDec{} = True
        hasPrototype _        = False
        mkPrototype (ExDec _ name ty _) = (name, ty)
        mkPrototype (FunDec _ name params retty _) = (name, FunTy (map snd params) retty)
        mkPrototype _ = error "ValDec has not prototype"


checkDecl :: Decl ID -> TypeCheck (Decl TypedID)
checkDecl (ExDec info name typ orig) =
    -- addBind name typ
    pure $ ExDec info (TypedID name typ) typ orig
checkDecl (ValDec info name Nothing val) = do
  val' <- checkExpr val
  addBind name (typeOf val')
  pure $ ValDec info (TypedID name (typeOf val')) Nothing val'
checkDecl (ValDec info name (Just typ) val) = do
    val' <- checkExpr val
    if typ == typeOf val'
      then do addBind name typ
              pure $ ValDec info (TypedID name typ) (Just typ) val'
      else throw info $
           "expected:" <+>
           ppr typ $+$ "actual:" <+> ppr (typeOf val')
checkDecl (FunDec info fn params retty body) = do
    fnty <- makeFnTy params retty
    -- addBind fn fnty
    mapM_ (uncurry addBind) params
    let fn' = TypedID fn fnty
    let params' = map (\(x, t) -> (TypedID x t, t)) params
    body' <- checkExpr body
    if typeOf body' == retty
      then pure $ FunDec info fn' params' retty body'
      else throw info $
           "expected:" <+>
           ppr retty $+$ "actual:" <+> ppr (typeOf body')
  where
    makeFnTy [] _   = throw info (text "void parameter is invalid")
    makeFnTy xs ret = pure $ FunTy (map snd xs) ret

checkExpr :: Expr ID -> TypeCheck (Expr TypedID)
checkExpr (Var info name) = Var info <$> getBind info name
checkExpr (Int info x) = pure $ Int info x
checkExpr (Float info x) = pure $ Float info x
checkExpr (Bool info x) = pure $ Bool info x
checkExpr (Char info x) = pure $ Char info x
checkExpr (String info x) = pure $ String info x
checkExpr (Unit info) = pure $ Unit info
checkExpr (Tuple info xs) = Tuple info <$> mapM checkExpr xs
checkExpr (Fn info params body) = do
  mapM_ (uncurry addBind) params
  let params' = map (\(x, t) -> (TypedID x t, t)) params
  body' <- checkExpr body
  pure $ Fn info params' body'
checkExpr (Call info fn args) = do
  fn' <- checkExpr fn
  args' <- mapM checkExpr args
  paramty <-
    case typeOf fn' of
      (FunTy p _) -> pure p
      _           -> throw info $ ppr fn' <+> "is not callable"
  unless (map typeOf args' == paramty)
    (throw info
      (text "expected:" <+>
        cat (punctuate (text ",") (map ppr paramty)) $+$
        text "actual:" <+>
        parens
        (cat $ punctuate (text ",") (map (ppr . typeOf) args'))))
  pure (Call info fn' args')
checkExpr (TupleAccess i tuple index) = do
  tuple' <- checkExpr tuple
  case typeOf tuple' of
    TupleTy xs ->
      when (index >= length xs) $
        throw i $ text "out of bounds:" <+> int index <+> ppr (TupleTy xs)
    t -> throw (Syntax.info tuple) $ text "expected: tuple" $+$
         text "actual:" <+> ppr t
  pure $ TupleAccess i tuple' index
checkExpr (BinOp info op x y) = do
    x' <- checkExpr x
    y' <- checkExpr y
    let (FunTy [px, py] _) = typeOfOp info op (typeOf x')
    when (typeOf x' /= px)
      (throw info $
        text "expected:" <+>
        ppr px $+$ text "actual:" <+> ppr (typeOf x'))
    when (typeOf y' /= py)
      (throw info $
        text "expected:" <+>
        ppr py $+$ text "actual:" <+> ppr (typeOf y'))
    pure (BinOp info op x' y')
checkExpr (Seq info e1 e2) = do
    e1' <- checkExpr e1
    unless (typeOf e1' == "Unit")
      (throw info $
        text "expected:" <+>
        text "Unit" $+$ "actual:" <+> ppr (typeOf e1'))
    Seq info e1' <$> checkExpr e2
checkExpr (Let info decls e) = do
    backup <- get
    mapM_ (uncurry addBind) (prototypes decls)
    decls' <- mapM checkDecl decls
    e' <- checkExpr e
    put backup
    pure (Let info decls' e')
checkExpr (If info c t f) = do
    c' <- checkExpr c
    t' <- checkExpr t
    f' <- checkExpr f
    case (typeOf c' == "Bool", typeOf t' == typeOf f') of
      (True, True) -> pure (If info c' t' f')
      (True, False) ->throw info $
                      text "expected:" <+>
                      ppr (typeOf t') $+$ text "actual:" <+>
                      ppr (typeOf f')
      _ -> throw info $
           text "expected:" <+>
           text "Bool" $+$ text "actual:" <+> ppr (typeOf c')
