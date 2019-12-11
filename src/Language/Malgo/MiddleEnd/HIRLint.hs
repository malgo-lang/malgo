{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.MiddleEnd.HIRLint (HIRLint) where

import           Control.Lens                (view, _1)
import           Language.Malgo.ID
import           Language.Malgo.IR.HIR
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Pretty
import           Language.Malgo.TypeRep.Type
import           Relude                      hiding (Type)

data HIRLint

instance Pass HIRLint (Expr Type TypedID) (Expr Type TypedID) where
  isDump _ = False
  trans e = usingReaderT [] (lintExpr e) >> pure e

defined :: TypedID -> ReaderT [TypedID] MalgoM ()
defined a =
  unlessM (elem a <$> ask)
  (malgoError $ pPrint a <+> "is not defined")

notDefined :: TypedID -> ReaderT [TypedID] MalgoM ()
notDefined a =
  unlessM (notElem a <$> ask)
  (malgoError $ pPrint a <+> "is already defined")

match :: (Pretty a, Pretty b, HasType a, HasType b) => a -> b -> ReaderT [TypedID] MalgoM ()
match a b = unless (typeOf a == typeOf b) $
  malgoError $ "expected:" <+> pPrint (typeOf a)
  $+$ "actual:" <+> pPrint (typeOf b)
  $+$ parens (fsep [pPrint a, colon, pPrint b])

lintExpr :: Expr Type TypedID -> ReaderT [TypedID] MalgoM Type
lintExpr e@(Call f xs) = do
  mapM_ defined (f:xs)
  paramtys <- getParamtys
  mapM_ (uncurry match) (zip paramtys argtys)
  pure $ typeOf e
  where
    fty = typeOf f
    argtys = map typeOf xs
    getParamtys =
      case fty of
        TyFun ps _ -> pure ps
        t -> malgoError $ pPrint t <+> ("is not callable: " <> parens (pPrint e))
lintExpr (Let name val body) = do
  notDefined name
  val' <- lintExpr val
  match name val'
  local (name:) $
    lintExpr body
lintExpr (LetRec fundecs body) =
  local (map (view _1) fundecs <>) $ do
    mapM_ lintFunDec fundecs
    lintExpr body
lintExpr (If c t f) = do
  match c TyBool
  _ <- lintExpr t
  lintExpr f
lintExpr e = pure $ typeOf e

lintFunDec :: (TypedID, [TypedID], Expr Type TypedID) -> ReaderT [TypedID] MalgoM Type
lintFunDec (_, ps, e) =
  local (ps <>) $
    lintExpr e
