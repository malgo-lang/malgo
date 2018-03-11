{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Malgo.TypeCheck
    ( typeCheck
    ) where

import qualified Data.Map.Strict        as Map
import           Language.Malgo.ID
import           Language.Malgo.Prelude
import           Language.Malgo.Syntax  hiding (info)
import qualified Language.Malgo.Syntax  as Syntax
import           Language.Malgo.Type
import           Language.Malgo.TypedID
import           Language.Malgo.Utils
import           Text.PrettyPrint

newtype TcEnv = TcEnv { _table :: Map.Map ID TypedID }

instance Env TcEnv where
    initEnv = TcEnv Map.empty

type TypeCheck m a = MalgoT TcEnv m a

typeCheck :: Monad m => Expr ID -> TypeCheck m (Expr TypedID)
typeCheck = checkExpr

throw :: Monad m => Info -> Doc -> TypeCheck m a
throw info mes = throwError $ TypeCheckError info mes

addBind :: Monad m => ID -> Type -> TypeCheck m ()
addBind name typ =
    modify $ \e -> e {_table = Map.insert name (TypedID name typ) (_table e)}

getBind :: Monad m => Info -> ID -> TypeCheck m TypedID
getBind info name = do
    t <- gets _table
    case lookup name t of
        Just x  -> pure x
        Nothing -> throw info (pretty name <+> text "is not defined")

checkDecl :: Monad m => Decl ID -> TypeCheck m (Decl TypedID)
checkDecl (ExDec info name typ orig) = do
    addBind name typ
    pure $ ExDec info (TypedID name typ) typ orig
checkDecl (ValDec info name Nothing val) = do
  val' <- checkExpr val
  addBind name (typeOf val')
  pure $ ValDec info (TypedID name (typeOf val')) Nothing val'
checkDecl (ValDec info name (Just typ) val) = do
    val' <- checkExpr val
    if typ == typeOf val'
        then do
            addBind name typ
            pure $ ValDec info (TypedID name typ) (Just typ) val'
        else throw
                 info
                 ("expected:" <+>
                  pretty typ $+$ "actual:" <+> pretty (typeOf val'))
checkDecl (FunDec info fn params retty body) = do
    fnty <- makeFnTy params retty
    addBind fn fnty
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
        -- makeFnTy [(_, t)] ret = pure $ FunTy t ret
    makeFnTy xs ret = pure $ FunTy (map snd xs) ret

checkExpr :: Monad m => Expr ID -> TypeCheck m (Expr TypedID)
checkExpr (Var info name) = Var info <$> getBind info name
checkExpr (Int info x) = pure $ Int info x
checkExpr (Float info x) = pure $ Float info x
checkExpr (Bool info x) = pure $ Bool info x
checkExpr (Char info x) = pure $ Char info x
checkExpr (String info x) = pure $ String info x
checkExpr (Unit info) = pure $ Unit info
checkExpr (Tuple info xs) = do
  xs' <- mapM checkExpr xs
  pure $ Tuple info xs'
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
    if map typeOf args' == paramty -- 引数が複数あるとき
     -- -- | (TupleTy (map typeOf args') == TupleTy [paramty]) -- 引数が1つのとき
        then pure $ Call info fn' args'
        else throw
                 info
                 (text "expected:" <+>
                  cat (punctuate (text ",") (map pretty paramty)) $+$
                  text "actual:" <+>
                  parens
                      (cat $ punctuate (text ",") (map (pretty . typeOf) args')))
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
    when
        (typeOf x' /= px)
        (throw info $
         text "expected:" <+>
         pretty px $+$ text "actual:" <+> pretty (typeOf x'))
    when
        (typeOf y' /= py)
        (throw info $
         text "expected:" <+>
         pretty py $+$ text "actual:" <+> pretty (typeOf y'))
    pure (BinOp info op x' y')
checkExpr (Seq info e1 e2) = do
    e1' <- checkExpr e1
    if typeOf e1' == "Unit"
        then Seq info e1' <$> checkExpr e2
        else throw info $
             text "expected:" <+>
             text "Unit" $+$ "actual:" <+> pretty (typeOf e1')
checkExpr (Let info decls e) = do
    backup <- get
    decls' <- mapM checkDecl decls
    e' <- checkExpr e
    put backup
    pure (Let info decls' e')
checkExpr (If info c t f) = do
    c' <- checkExpr c
    t' <- checkExpr t
    f' <- checkExpr f
    if typeOf c' == "Bool"
        then if typeOf t' == typeOf f'
                 then pure (If info c' t' f')
                 else throw info $
                      text "expected:" <+>
                      pretty (typeOf t') $+$ text "actual:" <+>
                      pretty (typeOf f')
        else throw info $
             text "expected:" <+>
             text "Bool" $+$ text "actual:" <+> pretty (typeOf c')
