{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Malgo.FrontEnd.Rename
  ( Rename
  )
where

import           Language.Malgo.ID
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Prelude
import           Language.Malgo.Pretty

import           Language.Malgo.IR.Syntax
                                         hiding ( info )

import           Language.Malgo.TypeRep.SType

import           Language.Malgo.FrontEnd.Info

import           Control.Monad.Cont
import           Control.Lens            hiding ( ix
                                                , op
                                                , lens
                                                )

data Known = Known { _var :: Map String (ID ())
                   , _tyVar :: Map String (ID ())
                   }

makeLenses ''Known

data Rename

instance Pass Rename (Expr String) (Expr (ID ())) where
  passName = "Rename"
  isDump   = dumpRenamed
  trans s = runReaderT (renameExpr s) $ Known mempty mempty

withKnowns :: (MonadUniq m, MonadReader Known m)
           => ASetter' Known (Map String (ID ()))
           -> [String]
           -> m a
           -> m a
withKnowns lens ks m = do
  vs <- mapM (newID ()) ks
  local (over lens (fromList (zip ks vs) <>)) m

getID :: MonadReader Known m
      => Info
      -> Getting (Map String (ID ())) Known (Map String (ID ()))
      -> String
      -> m (ID ())
getID info lens name = do
  k <- view lens
  case lookup name (k :: Map String (ID ())) of
    Just x -> pure x
    Nothing ->
      errorDoc $ "error(rename):" <+> pPrint (info :: Info) <+> pPrint name <+> "is not defined"

renameExpr :: (MonadUniq m, MonadReader Known m) => Expr String -> m (Expr (ID ()))
renameExpr (Var    info name        ) = Var info <$> getID info var name
renameExpr (Int    info x           ) = pure $ Int info x
renameExpr (Float  info x           ) = pure $ Float info x
renameExpr (Bool   info x           ) = pure $ Bool info x
renameExpr (Char   info x           ) = pure $ Char info x
renameExpr (String info x           ) = pure $ String info x
renameExpr (Tuple  info xs          ) = Tuple info <$> mapM renameExpr xs
renameExpr (Array  info xs          ) = Array info <$> mapM renameExpr xs
renameExpr (MakeArray info init size) = MakeArray info <$> renameExpr init <*> renameExpr size
renameExpr (ArrayRead info arr  ix  ) = ArrayRead info <$> renameExpr arr <*> renameExpr ix
renameExpr (ArrayWrite info arr ix val) =
  ArrayWrite info <$> renameExpr arr <*> renameExpr ix <*> renameExpr val
renameExpr (Fn info params body) =
  withKnowns var (map fst params)
    $   Fn info
    <$> mapM (bitraverse (getID info var) (mapM (renameSType info))) params
    <*> renameExpr body
renameExpr (Call info fn args) = Call info <$> renameExpr fn <*> mapM renameExpr args
renameExpr (Seq info e1 e2) = Seq info <$> renameExpr e1 <*> renameExpr e2
renameExpr (Let info0 (ValDec info1 name typ val) e) = do
  val' <- renameExpr val
  withKnowns var [name]
    $   withKnowns tyVar (ordNub $ concat $ maybeToList $ fmap toList typ)
    $   Let info0
    <$> (ValDec info1 <$> getID info1 var name <*> mapM (renameSType info1) typ <*> pure val')
    <*> renameExpr e
renameExpr (Let info0 (FunDec fs) e) = withKnowns var (map (view _2) fs) $ do
  fs' <- mapM renameFunDec fs
  Let info0 (FunDec fs') <$> renameExpr e
 where
  renameFunDec (info, fn, params, retty, body) = do
    fn' <- getID info var fn
    withKnowns var (map fst params)
      $ withKnowns tyVar (ordNub $ concatMap toList $ mapMaybe snd params)
      $ do
          params' <- mapM (bitraverse (getID info var) (mapM (renameSType info))) params
          body'   <- renameExpr body
          retty'  <- mapM (renameSType info) retty
          pure (info, fn', params', retty', body')
renameExpr (Let info0 (ExDec info1 name typ orig) e) =
  withKnowns var [name]
    $   withKnowns tyVar (ordNub $ toList typ)
    $   Let info0
    <$> (ExDec info1 <$> getID info1 var name <*> renameSType info1 typ <*> pure orig)
    <*> renameExpr e
renameExpr (If    info c  t f) = If info <$> renameExpr c <*> renameExpr t <*> renameExpr f
renameExpr (BinOp info op x y) = BinOp info op <$> renameExpr x <*> renameExpr y
renameExpr (Match info scrutinee clauses) =
  Match info <$> renameExpr scrutinee <*> mapM (renameClause info) clauses

renameClause :: (MonadUniq m, MonadReader Known m)
             => Info
             -> (Pat String, Expr String)
             -> m (Pat (ID ()), Expr (ID ()))
renameClause info (p, e) = runContT (renamePat p) $ \p' -> (p', ) <$> renameExpr e
 where
  renamePat (VarP   x ) = ContT $ \k -> withKnowns var [x] $ VarP <$> getID info var x >>= k
  renamePat (TupleP ps) = TupleP <$> mapM renamePat ps

renameSType :: (MonadReader Known m, MonadUniq m) => Info -> SType String -> m (SType (ID ()))
renameSType i (TyVar x)    = TyVar <$> getID i tyVar x
renameSType _ TyInt        = pure TyInt
renameSType _ TyFloat      = pure TyFloat
renameSType _ TyBool       = pure TyBool
renameSType _ TyChar       = pure TyChar
renameSType _ TyString     = pure TyString
renameSType i (TyFun ps r) = TyFun <$> mapM (renameSType i) ps <*> renameSType i r
renameSType i (TyTuple xs) = TyTuple <$> mapM (renameSType i) xs
renameSType i (TyArray x ) = TyArray <$> renameSType i x
