{-# LANGUAGE TemplateHaskell #-}
module Language.Malgo.InsertRET where

import           Control.Lens
import           Control.Monad.State
import           Language.Malgo.HIR    (Id (..))
import           Language.Malgo.MIR    hiding (Env)
import           Language.Malgo.Syntax

newtype Env = Env { _retCount :: Int }
  deriving (Eq, Show)
makeLenses ''Env

newRet :: String -> State Env Id
newRet hint = do
  c <- use retCount
  retCount .= c + 1
  return (Sym $ "$R" ++ show c ++ "_" ++ hint)

trans d = runState (insertRET d) (Env 0)

insertRET :: DECL -> State Env DECL
insertRET (DEFUN fn retTy params body) =
  DEFUN fn retTy params <$> insertRET_B body
insertRET e = return e

insertRET_B :: BLOCK -> State Env BLOCK
insertRET_B (BLOCK (Sym label) es) = do
  ret <- newRet label
  BLOCK (Sym label) <$> insertRET_B' ret es

insertRET_B' :: Id -> [EXPR] -> State Env [EXPR]
insertRET_B' _ [(IF ret c t f, ty)] = do
  t' <- insertRET_B t
  f' <- insertRET_B f
  return [(IF ret c t' f', ty)]
insertRET_B' ret [(e, t)] =
  return [(LET ret t (e, t), UnitTy), (RET ret t, UnitTy)]
insertRET_B' ret (e:es) = (insertIFRET e :) <$> insertRET_B' ret es
insertRET_B' _ _ = undefined

insertIFRET :: EXPR -> EXPR
insertIFRET (LET ret ty (IF _ c t f, _), _) =
  let t' = insertIFRETB ret ty t
  in let f' = insertIFRETB ret ty f
  in (IF ret c t' f', ty)
insertIFRET e = e

insertIFRETB :: Id -> Type -> BLOCK -> BLOCK
insertIFRETB ret ty (BLOCK label es) = BLOCK label $ insertIFRETB' ret ty es

insertIFRETB' :: Id -> Type -> [EXPR] -> [EXPR]
insertIFRETB' ret ty [e] =
  [(IFRET ret ty e, UnitTy)]
insertIFRETB' ret ty (e:es) = insertIFRET e : insertIFRETB' ret ty es
insertIFRETB' _ _ _ = undefined
