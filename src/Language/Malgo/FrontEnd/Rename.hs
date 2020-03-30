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

import           Language.Malgo.Id
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Prelude  hiding ( lens
                                                , ix
                                                , op
                                                )
import           Language.Malgo.Pretty

import           Language.Malgo.IR.Syntax

import           Language.Malgo.TypeRep.SType

import           Control.Monad.Cont
import qualified Data.Map                      as Map
import           Text.Parsec.Pos                ( SourcePos )

data Known = Known { _var :: Map String (Id ())
                   , _tyVar :: Map String (Id ())
                   }

makeLenses ''Known

data Rename

instance Pass Rename (Expr String) (Expr (Id ())) where
  passName = "Rename"
  isDump   = dumpRenamed
  trans s = runReaderT (renameExpr s) $ Known mempty mempty

withKnowns :: (MonadUniq m, MonadReader Known m)
           => ASetter' Known (Map String (Id ()))
           -> [String]
           -> m a
           -> m a
withKnowns lens ks m = do
  vs <- mapM (newId ()) ks
  local (over lens (Map.fromList (zip ks vs) <>)) m

getId :: (MonadReader s m, MonadMalgo m)
      => SourcePos
      -> Getting (Map String (Id ())) s (Map String (Id ()))
      -> String
      -> m (Id ())
getId pos lens name = do
  k <- view lens
  case view (at name) k of
    Just x  -> pure x
    Nothing -> malgoError pos "rename" $ pPrint name <+> "is not defined"

renameExpr :: (MonadMalgo m, MonadUniq m, MonadReader Known m) => Expr String -> m (Expr (Id ()))
renameExpr (Var    pos name     ) = Var pos <$> getId pos var name
renameExpr (Int    pos x        ) = pure $ Int pos x
renameExpr (Float  pos x        ) = pure $ Float pos x
renameExpr (Bool   pos x        ) = pure $ Bool pos x
renameExpr (Char   pos x        ) = pure $ Char pos x
renameExpr (String pos x        ) = pure $ String pos x
renameExpr (Tuple  pos xs       ) = Tuple pos <$> mapM renameExpr xs
renameExpr (Array  pos xs       ) = Array pos <$> mapM renameExpr xs
renameExpr (MakeArray pos x   n ) = MakeArray pos <$> renameExpr x <*> renameExpr n
renameExpr (ArrayRead pos arr ix) = ArrayRead pos <$> renameExpr arr <*> renameExpr ix
renameExpr (ArrayWrite pos arr ix val) =
  ArrayWrite pos <$> renameExpr arr <*> renameExpr ix <*> renameExpr val
renameExpr (Fn pos params body) =
  withKnowns var (map fst params)
    $   Fn pos
    <$> mapM (bitraverse (getId pos var) (mapM (renameSType pos))) params
    <*> renameExpr body
renameExpr (Call pos fn args) = Call pos <$> renameExpr fn <*> mapM renameExpr args
renameExpr (Seq pos e1 e2) = Seq pos <$> renameExpr e1 <*> renameExpr e2
renameExpr (Let pos0 (ValDec pos1 name typ val) e) = do
  val' <- renameExpr val
  withKnowns var [name]
    $   withKnowns tyVar (ordNub $ concat $ maybeToList $ fmap toList typ)
    $   Let pos0
    <$> (ValDec pos1 <$> getId pos1 var name <*> mapM (renameSType pos1) typ <*> pure val')
    <*> renameExpr e
renameExpr (Let pos0 (FunDec fs) e) = withKnowns var (map (view _2) fs) $ do
  fs' <- mapM renameFunDec fs
  Let pos0 (FunDec fs') <$> renameExpr e
 where
  renameFunDec (pos, fn, params, retty, body) = do
    fn' <- getId pos var fn
    withKnowns var (map fst params)
      $ withKnowns tyVar (ordNub $ concatMap toList $ mapMaybe snd params)
      $ do
          params' <- mapM (bitraverse (getId pos var) (mapM (renameSType pos))) params
          body'   <- renameExpr body
          retty'  <- mapM (renameSType pos) retty
          pure (pos, fn', params', retty', body')
renameExpr (Let pos0 (ExDec pos1 name typ orig) e) =
  withKnowns var [name]
    $   withKnowns tyVar (ordNub $ toList typ)
    $   Let pos0
    <$> (ExDec pos1 <$> getId pos1 var name <*> renameSType pos1 typ <*> pure orig)
    <*> renameExpr e
renameExpr (If    pos c  t f) = If pos <$> renameExpr c <*> renameExpr t <*> renameExpr f
renameExpr (BinOp pos op x y) = BinOp pos op <$> renameExpr x <*> renameExpr y
renameExpr (Match pos scrutinee clauses) =
  Match pos <$> renameExpr scrutinee <*> mapM (renameClause pos) clauses

renameClause :: (MonadMalgo m, MonadUniq m, MonadReader Known m)
             => SourcePos
             -> (Pat String, Expr String)
             -> m (Pat (Id ()), Expr (Id ()))
renameClause pos (p, e) = runContT (renamePat p) $ \p' -> (p', ) <$> renameExpr e
 where
  renamePat (VarP   x ) = ContT $ \k -> withKnowns var [x] $ VarP <$> getId pos var x >>= k
  renamePat (TupleP ps) = TupleP <$> mapM renamePat ps

renameSType :: (MonadMalgo m, MonadReader Known m, MonadUniq m)
            => SourcePos
            -> SType String
            -> m (SType (Id ()))
renameSType pos (TyVar x)    = TyVar <$> getId pos tyVar x
renameSType _   TyInt        = pure TyInt
renameSType _   TyFloat      = pure TyFloat
renameSType _   TyBool       = pure TyBool
renameSType _   TyChar       = pure TyChar
renameSType _   TyString     = pure TyString
renameSType pos (TyFun ps r) = TyFun <$> mapM (renameSType pos) ps <*> renameSType pos r
renameSType pos (TyTuple xs) = TyTuple <$> mapM (renameSType pos) xs
renameSType pos (TyArray x ) = TyArray <$> renameSType pos x
