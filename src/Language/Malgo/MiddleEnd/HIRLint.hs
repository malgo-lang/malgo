{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Malgo.MiddleEnd.HIRLint
  ( HIRLint
  )
where

import           Language.Malgo.Id
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Prelude
import           Language.Malgo.Pretty

import           Language.Malgo.IR.HIR

import           Language.Malgo.TypeRep.Type

import           Control.Exception              ( assert )
import qualified Data.Text.Lazy                as TL
import           Text.PrettyPrint.HughesPJClass ( ($+$)
                                                , parens
                                                , fsep
                                                , colon
                                                )

data HIRLint

instance Pass HIRLint (Expr (Id Type)) (Expr (Id Type)) where
  passName = "HIRLint"
  isDump _ = False
  trans e = runReaderT (lintExpr e) [] >> pure e

defined :: Id Type -> ReaderT [Id Type] MalgoM ()
defined a = (assert ?? ()) <$> (elem a <$> ask)

notDefined :: Id Type -> ReaderT [Id Type] MalgoM ()
notDefined a = (assert ?? ()) <$> (notElem a <$> ask)

match :: (Pretty a, Pretty b, HasType a, HasType b) => a -> b -> ReaderT [Id Type] MalgoM ()
match a b = case (typeOf a, typeOf b) of
  (TyMeta _, _       )                    -> pure ()
  (_       , TyMeta _)                    -> pure ()
  (TyApp c1 ts1, TyApp c2 ts2) | c1 == c2 -> zipWithM_ match ts1 ts2
  (t1, t2) ->
    unless (t1 == t2)
      $   errorDoc
      $   "expected:"
      <+> pPrint (typeOf a)
      $+$ "actual:"
      <+> pPrint (typeOf b)
      $+$ parens (fsep [pPrint a, colon, pPrint b])

lintExpr :: Expr (Id Type) -> ReaderT [Id Type] MalgoM Type
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
    t                   -> errorDoc $ pPrint t <+> ("is not callable: " <> parens (pPrint e))
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

lintFunDec :: Def (Id Type) -> ReaderT [Id Type] MalgoM Type
lintFunDec Def { params, expr } = local (params <>) $ lintExpr expr
