{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Malgo.TypeCheck
    ( typeCheck
    ) where

import qualified Data.Map.Strict        as Map
import           Language.Malgo.ID
import           Language.Malgo.Monad
import           Language.Malgo.Prelude
import           Language.Malgo.Syntax  hiding (info)
import qualified Language.Malgo.Syntax  as Syntax
import           Language.Malgo.Type
import           Language.Malgo.TypedID
import           Language.Malgo.Utils
import           Text.PrettyPrint

data TcEnv = TcEnv { _table      :: Map.Map ID TypedID
                   , _uniqSupply :: Int
                   }
  deriving Generic

instance Default TcEnv

instance HasUniqSupply TcEnv where
  uniqSupply = lens _uniqSupply (\s i -> s { _uniqSupply = i })

type TypeCheck a = Malgo TcEnv a

typeCheck :: Expr ID -> TypeCheck (Expr TypedID)
typeCheck = checkExpr

throw :: Info -> Doc -> TypeCheck a
throw info mes = malgoError $ "error(typecheck):" <+> pretty info <+> mes

addBind :: ID -> Type -> TypeCheck ()
addBind name typ =
    modify $ \e -> e {_table = Map.insert name (TypedID name typ) (_table e)}

getBind :: Info -> ID -> TypeCheck TypedID
getBind info name = do
    t <- gets _table
    maybe (throw info (pretty name <+> "is not defined")) return (lookup name t)

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
           pretty typ $+$ "actual:" <+> pretty (typeOf val')
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
           pretty retty $+$ "actual:" <+> pretty (typeOf body')
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
      _           -> throw info $ pretty fn' <+> "is not callable"
  unless (map typeOf args' == paramty)
    (throw info
      (text "expected:" <+>
        cat (punctuate (text ",") (map pretty paramty)) $+$
        text "actual:" <+>
        parens
        (cat $ punctuate (text ",") (map (pretty . typeOf) args'))))
  pure (Call info fn' args')
checkExpr (TupleAccess i tuple index) = do
  tuple' <- checkExpr tuple
  case typeOf tuple' of
    TupleTy xs ->
      when (index >= length xs) $
        throw i $ text "out of bounds:" <+> int index <+> pretty (TupleTy xs)
    t -> throw (Syntax.info tuple) $ text "expected: tuple" $+$
         text "actual:" <+> pretty t
  pure $ TupleAccess i tuple' index
checkExpr (BinOp info op x y) = do
    x' <- checkExpr x
    y' <- checkExpr y
    let (FunTy [px, py] _) = typeOfOp info op (typeOf x')
    when (typeOf x' /= px)
      (throw info $
        text "expected:" <+>
        pretty px $+$ text "actual:" <+> pretty (typeOf x'))
    when (typeOf y' /= py)
      (throw info $
        text "expected:" <+>
        pretty py $+$ text "actual:" <+> pretty (typeOf y'))
    pure (BinOp info op x' y')
checkExpr (Seq info e1 e2) = do
    e1' <- checkExpr e1
    unless (typeOf e1' == "Unit")
      (throw info $
        text "expected:" <+>
        text "Unit" $+$ "actual:" <+> pretty (typeOf e1'))
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
                      pretty (typeOf t') $+$ text "actual:" <+>
                      pretty (typeOf f')
      _ -> throw info $
           text "expected:" <+>
           text "Bool" $+$ text "actual:" <+> pretty (typeOf c')
