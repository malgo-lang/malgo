{-# LANGUAGE DataKinds #-}
module Language.Malgo.Assoc where

-- ネストしたLetの簡約
import           Language.Malgo.HIR
import           Language.Malgo.Syntax (Type)

-- flatten :: HIR 'KNormal -> HIR 'KNormal
flatten :: HIR a -> HIR a
flatten (HIR x) = HIR $ flattenDECL x

-- flatten' :: DECL 'KNormal -> DECL 'KNormal
flattenDECL :: DECL a -> DECL a
flattenDECL (DEF i t v)               = DEF i t (flattenEXPR v)
flattenDECL (DEFUN i ret params body) = DEFUN i ret params (flattenEXPR body)
flattenDECL (EXDEF i t)               = EXDEF i t
flattenDECL (EXDEFUN i ret params)    = EXDEFUN i ret params

-- flattenEXPR :: (EXPR' 'KNormal, Type) -> (EXPR' 'KNormal, Type)
flattenEXPR :: (EXPR' a, Type) -> (EXPR' a, Type)
flattenEXPR (e, t) = (flattenEXPR' e, t)

-- flattenEXPR' :: EXPR' 'KNormal -> EXPR' 'KNormal
flattenEXPR' :: EXPR' a -> EXPR' a
flattenEXPR' (IF c e1 e2)    = IF c (flattenEXPR e1) (flattenEXPR e2)
flattenEXPR' (LET x xt e1 e2) = insert (flattenEXPR e1)
  where
    -- insert :: (EXPR 'KNormal -> EXPR' 'KNormal)
    insert (LET y yt e3 e4, t) = LET y yt e3 (insert e4, t)
    insert e                   = LET x xt e (flattenEXPR e2)
flattenEXPR' x = x
