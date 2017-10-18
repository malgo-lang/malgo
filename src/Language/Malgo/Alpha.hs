{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
module Language.Malgo.Alpha where

-- アルファ変換

import           Control.Applicative ()
import           Control.Lens
import           Control.Monad.State
import           Data.Maybe          (fromMaybe)
import           Language.Malgo.HIR

data Env = Env { _idMap   :: [(Id, Id)]
               , _idCount :: Int
               }
  deriving (Show, Eq)
makeLenses ''Env

newId :: Id -> State Env Id
newId (Sym hint) = do
  c <- use idCount
  idCount .= (c + 1)
  return $ Sym ("$a" ++ show c ++ "_" ++ hint)

addBind :: (Id, Id) -> State Env ()
addBind (x, y) = do
  m <- use idMap
  idMap .= (x, y) : m

find :: Id -> State Env Id
find x = do
  env <- use idMap
  let x' = lookup x env
  return $ fromMaybe x x'

transDECL :: DECL 'KNormal -> State Env (DECL 'KNormal)
transDECL (DEF i t v) = do
  DEF i t <$> transEXPR v
transDECL (DEFUN fn retTy params body) = do
  -- fn' <- newId fn
  -- addBind (fn, fn')
  addParams params
  env <- get
  DEFUN fn
    retTy
    (zip
     (evalState (mapM (find . fst) params) env)
     (map snd params))
    <$> transEXPR body
  where addParams [] = return ()
        addParams (x:xs) = do
          x' <- newId (fst x)
          addBind (fst x, x')
          addParams xs
transDECL e = return e

transEXPR :: EXPR 'KNormal -> State Env (EXPR 'KNormal)
transEXPR (e, t) = (,) <$> transEXPR' e <*> pure t

transEXPR' :: EXPR' 'KNormal -> State Env (EXPR' 'KNormal)
transEXPR' (BINOP op e1 e2) = BINOP op <$> transEXPR e1 <*> transEXPR e2
transEXPR' (IF c t f) = IF <$> transEXPR c <*> transEXPR t <*> transEXPR f
transEXPR' (LET i t v e) = do
  v' <- transEXPR v
  i' <- newId i
  addBind (i, i')
  LET i' t v' <$> transEXPR e
transEXPR' (VAR x) = VAR <$> find x
transEXPR' (CALL fn args) = CALL <$> find fn <*> mapM transEXPR args
transEXPR' x = return x

trans :: HIR 'KNormal -> (HIR 'KNormal, Env)
trans (HIR d) = runState (transDECL d) (Env [] 0) & _1 %~ HIR
