{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Malgo.MiddleEnd.KNormal (knormal, checkKNormalized) where

import qualified Data.Map                        as Map
import           Language.Malgo.FrontEnd.Loc
import           Language.Malgo.FrontEnd.RnTcEnv
import           Language.Malgo.Id
import           Language.Malgo.IR.AST
import           Language.Malgo.MiddleEnd.TypeOf
import           Language.Malgo.Monad
import           Language.Malgo.Type
import           Universum                       hiding (Type)

knormal :: (MonadMalgo m, MonadState RnTcEnv m) => [Decl Id] -> m [Decl Id]
knormal = mapM knDecl

knDecl :: (MonadMalgo m, MonadState RnTcEnv m) => Decl Id -> m (Decl Id)
knDecl (ScDef ss f xs e) = ScDef ss f xs <$> knExpr e
knDecl d                 = return d

knExpr :: (MonadMalgo m, MonadState RnTcEnv m) => Expr Id -> m (Expr Id)
knExpr = undefined

insertLet :: (MonadMalgo m, MonadState RnTcEnv m) => Expr Id -> (Id -> m (Expr Id)) -> m (Expr Id)
insertLet (Var _ x) k = k x
insertLet v k = do
  v' <- knExpr v
  x <- newId "k"
  t <- typeOf v'
  modify (over variableMap (Map.insert x t))
  e <- k x
  return $ Let (srcSpan v) (NonRec (srcSpan v) x (Just t) v') e

checkKNormalized :: [Decl Id] -> Bool
checkKNormalized = all ckDecl
  where
    ckDecl TypeDef{}       = True
    ckDecl ScAnn{}         = True
    ckDecl (ScDef _ _ _ e) = ckExpr e

    isVar Var{} = True
    isVar _     = False

    ckExpr Var{}                        = True
    ckExpr Literal{}                    = True
    ckExpr (BinOp _ _ x y)              = isVar x && isVar y
    ckExpr (If _ c t f)                 = isVar c && ckExpr t && ckExpr f
    ckExpr (Let _ (NonRec _ _ _ v) e)   = ckExpr v && ckExpr e
    ckExpr (Let _ (Rec _ _ _ _ v) e)    = ckExpr v && ckExpr e
    ckExpr (Let _ (TuplePat _ _ _ v) e) = ckExpr v && ckExpr e
    ckExpr (Apply _ x y)                = isVar x && isVar y
    ckExpr (Tuple _ xs)                 = all isVar xs
