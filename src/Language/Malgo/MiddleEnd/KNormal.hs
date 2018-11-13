{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Malgo.MiddleEnd.KNormal
  ( knormal
  , checkKNormalized
  )
where

import           Control.Monad.Cont
import qualified Data.Map                      as Map
import           Language.Malgo.FrontEnd.Loc
import           Language.Malgo.FrontEnd.RnTcEnv
import           Language.Malgo.Id
import           Language.Malgo.IR.AST
import           Language.Malgo.MiddleEnd.TypeOf
import           Language.Malgo.Monad
import           Universum               hiding ( Type )

knormal :: (MonadMalgo m, MonadState RnTcEnv m) => [Decl Id] -> m [Decl Id]
knormal = mapM knDecl

knDecl :: (MonadMalgo m, MonadState RnTcEnv m) => Decl Id -> m (Decl Id)
knDecl (ScDef ss f xs e) = ScDef ss f xs <$> knExpr e
knDecl d                 = return d

knExpr :: (MonadMalgo m, MonadState RnTcEnv m) => Expr Id -> m (Expr Id)
knExpr (Apply ss x y) = insertLet x $ \x' -> insertLet y $ return . Apply ss x'
knExpr (BinOp ss op x y) =
  insertLet x $ \x' -> insertLet y $ return . BinOp ss op x'
knExpr (If ss c t f) = insertLet c $ \c' -> If ss c' <$> knExpr t <*> knExpr f
knExpr (Let ss0 (NonRec ss1 x mts v) e) =
  Let ss0 <$> (NonRec ss1 x mts <$> knExpr v) <*> knExpr e
knExpr (Let ss0 (Rec ss1 x ps mts v) e) =
  Let ss0 <$> (Rec ss1 x ps mts <$> knExpr v) <*> knExpr e
knExpr (Let ss0 (TuplePat ss1 xs mts v) e) =
  Let ss0 <$> (TuplePat ss1 xs mts <$> knExpr v) <*> knExpr e
knExpr (Tuple ss xs) = go xs [] $ \xs' -> return $ Tuple ss xs'
 where
  go []       acc k = k $ reverse acc
  go (y : ys) acc k = insertLet y $ \y' -> go ys (y' : acc) k
knExpr x = return x

insertLet
  :: (MonadMalgo m, MonadState RnTcEnv m)
  => Expr Id
  -> (Expr Id -> m (Expr Id))
  -> m (Expr Id)
insertLet x@Var{} k = k x
insertLet v       k = do
  v' <- knExpr v
  x  <- newId "k"
  t  <- typeOf v'
  modify (over variableMap (Map.insert x t))
  e <- k (Var (srcSpan v) x)
  return $ Let (srcSpan v) (NonRec (srcSpan v) x (Just t) v') e

checkKNormalized :: [Decl Id] -> Bool
checkKNormalized = all ckDecl
 where
  ckDecl TypeDef{}       = True
  ckDecl ScAnn{}         = True
  ckDecl (ScDef _ _ _ e) = ckExpr e

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
