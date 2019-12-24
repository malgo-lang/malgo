{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.MiddleEnd.HIRLint
  ( HIRLint
  )
where

import           Language.Malgo.ID
import           Language.Malgo.IR.HIR
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Pretty
import           Language.Malgo.TypeRep.Type
import           Language.Malgo.Prelude

data HIRLint

instance Pass HIRLint (Expr Type (ID Type)) (Expr Type (ID Type)) where
  isDump _ = False
  trans e = usingReaderT [] (lintExpr e) >> pure e

defined :: ID Type -> ReaderT [ID Type] MalgoM ()
defined a = unlessM (elem a <$> ask) (errorDoc $ pPrint a <+> "is not defined")

notDefined :: ID Type -> ReaderT [ID Type] MalgoM ()
notDefined a =
  unlessM (notElem a <$> ask) (errorDoc $ pPrint a <+> "is already defined")

match
  :: (Pretty a, Pretty b, HasType a, HasType b)
  => a
  -> b
  -> ReaderT [ID Type] MalgoM ()
match a b =
  unless (typeOf a == typeOf b)
    $   errorDoc
    $   "expected:"
    <+> pPrint (typeOf a)
    $+$ "actual:"
    <+> pPrint (typeOf b)
    $+$ parens (fsep [pPrint a, colon, pPrint b])

lintExpr :: Expr Type (ID Type) -> ReaderT [ID Type] MalgoM Type
lintExpr e@(Call f xs) = do
  mapM_ defined (f : xs)
  paramtys <- getParamtys
  mapM_ (uncurry match) (zip paramtys argtys)
  pure $ typeOf e
 where
  fty         = typeOf f
  argtys      = map typeOf xs
  getParamtys = case fty of
    TyApp FunC (_ : ps) -> pure ps
    t -> errorDoc $ pPrint t <+> ("is not callable: " <> parens (pPrint e))
lintExpr (Let name val body) = do
  notDefined name
  val' <- lintExpr val
  match name val'
  local (name :) $ lintExpr body
lintExpr (LetRec fundecs body) = local (map name fundecs <>) $ do
  mapM_ lintFunDec fundecs
  lintExpr body
lintExpr (If c t f) = do
  match c $ TyApp BoolC []
  _ <- lintExpr t
  lintExpr f
lintExpr e = pure $ typeOf e

lintFunDec :: Def Type (ID Type) -> ReaderT [ID Type] MalgoM Type
lintFunDec Def { params, expr } = local (params <>) $ lintExpr expr
