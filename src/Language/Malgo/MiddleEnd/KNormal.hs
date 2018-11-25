{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
module Language.Malgo.MiddleEnd.KNormal
  ( knormal
  , checkKNormalized
  )
where

import           Control.Monad.Cont
import qualified Data.Map                          as Map
import           Language.Malgo.FrontEnd.Loc
import           Language.Malgo.FrontEnd.RnTcEnv
import           Language.Malgo.FrontEnd.TypeCheck (unfoldTyMetaScheme)
import           Language.Malgo.Id
import           Language.Malgo.IR.AST
import           Language.Malgo.MiddleEnd.TypeOf
import           Language.Malgo.Monad
import           Universum                         hiding (Type)

knormal
  :: (MonadMalgo m, MonadState RnTcEnv m)
  => Program Id
  -> m [(Id, [Id], Expr Id)]
knormal (Program ds) = map catMaybes $ mapM knDecl ds

knDecl
  :: (MonadMalgo m, MonadState RnTcEnv m)
  => Decl Id
  -> m (Maybe (Id, [Id], Expr Id))
knDecl (ScDef _ f xs e) = Just . (f, xs, ) <$> knExpr e
knDecl _                = return Nothing

knExpr :: (MonadMalgo m, MonadState RnTcEnv m) => Expr Id -> m (Expr Id)
knExpr (Apply ss x y) = do
  (letX, x') <- insertLet x
  (letY, y') <- insertLet y
  return $ letX $ letY $ Apply ss x' y'
knExpr (BinOp ss op x y) = do
  (letX, x') <- insertLet x
  (letY, y') <- insertLet y
  return $ letX $ letY $ BinOp ss op x' y'
knExpr (If ss c t f) = do
  (letC, c') <- insertLet c
  letC <$> (If ss c' <$> knExpr t <*> knExpr f)
knExpr (Let ss0 (NonRec ss1 x mts v) e) =
  Let ss0 <$> (NonRec ss1 x mts <$> knExpr v) <*> knExpr e
knExpr (Let ss0 (Rec ss1 x ps mts v) e) =
  Let ss0 <$> (Rec ss1 x ps mts <$> knExpr v) <*> knExpr e
knExpr (Let ss0 (TuplePat ss1 xs mts v) e) =
  Let ss0 <$> (TuplePat ss1 xs mts <$> knExpr v) <*> knExpr e
knExpr (Tuple ss xs) = do
  (letXs, xs') <- first (foldr (.) id) <$> mapAndUnzipM insertLet xs
  return $ letXs $ Tuple ss xs'
knExpr x = return x

insertLet :: (MonadMalgo m, MonadState RnTcEnv m) => Expr Id -> m (Expr Id -> Expr Id, Expr Id)
insertLet x@Var{} = return (id, x)
insertLet v = do
  v' <- knExpr v
  x <- newId "k"
  t <- unfoldTyMetaScheme =<< typeOf v'
  modify (over variableMap (Map.insert x t))
  return (Let (srcSpan v) (NonRec (srcSpan v) x (Just t) v'), Var (srcSpan v) x)

checkKNormalized :: (Id, [Id], Expr Id) -> Bool
checkKNormalized = ckExpr . view _3
 where
  isVar Var{} = True
  isVar _     = False

  ckExpr Var{}                          = True
  ckExpr Literal{}                      = True
  ckExpr (BinOp _ _ x y               ) = isVar x && isVar y
  ckExpr (If    _ c t f               ) = isVar c && ckExpr t && ckExpr f
  ckExpr (Let   _ (NonRec _ _ _ v  ) e) = ckExpr v && ckExpr e
  ckExpr (Let   _ (Rec _ _ _ _ v   ) e) = ckExpr v && ckExpr e
  ckExpr (Let   _ (TuplePat _ _ _ v) e) = ckExpr v && ckExpr e
  ckExpr (Apply _ x                  y) = isVar x && isVar y
  ckExpr (Tuple _ xs                  ) = all isVar xs
