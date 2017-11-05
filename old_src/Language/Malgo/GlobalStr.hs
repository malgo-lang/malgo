{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Malgo.GlobalStr where

-- 文字列型のローカル変数をグローバル変数に持ち上げる
import           Control.Monad.State
import           Language.Malgo.HIR    (Id (..))
import           Language.Malgo.MIR    (BLOCK (..), DECL (..), EXPR, EXPR' (..))
import           Language.Malgo.Syntax (Type (..))

data TransState = TransState { defs   :: [DECL]
                             , strtab :: [(Id, Id)]
                             }
  deriving Show

newtype Trans a = Trans (State TransState a)
  deriving (Functor, Applicative, Monad, MonadState TransState)

runTrans :: Trans a -> [DECL]
runTrans (Trans m) = reverse . defs $ execState m (TransState [] [])

trans :: [DECL] -> [DECL]
trans xs = runTrans (mapM transDECL xs)

addDef :: DECL -> Trans ()
addDef def =
  modify $ \s -> s { defs = def : defs s }

addStrTab :: Id -> Id -> Trans ()
addStrTab old new = modify $ \s -> s { strtab = (old, new) : strtab s }

getVar :: Id -> Trans Id
getVar old = do
  stab <- gets strtab
  case (lookup old stab) of
    Just x  -> return x
    Nothing -> return old

transDECL :: DECL -> Trans DECL
transDECL (DEFUN fn retTy params body) = do
  body' <- transBLOCK fn body
  addDef (DEFUN fn retTy params body')
  return (DEFUN fn retTy params body')
transDECL d = addDef d >> return d

transBLOCK :: Id -> BLOCK -> Trans BLOCK
transBLOCK fn (BLOCK label xs) = do
  xs' <- mapM (transEXPR fn label) xs
  return $ BLOCK label xs'

transEXPR :: Id -> Id -> EXPR -> Trans EXPR
transEXPR (Sym fn) (Sym label) (LET (Sym name) StringTy (STRING x, _), _) = do
  addDef (DEF (Sym $ fn ++ label ++ name) StringTy (STRING x, StringTy))
  addStrTab (Sym name) (Sym $ fn ++ label ++ name)
  return (NOP, UnitTy)
transEXPR f l (LET name typ val, ty) = do
  val' <- transEXPR f l val
  return (LET name typ val', ty)
transEXPR _ _ (VAR name, ty) = do
  name' <- getVar name
  return (VAR name', ty)
transEXPR _ _ (CALL fn args, ty) = do
  args' <- mapM (getVar . fst) args
  return (CALL fn (zip args' (map snd args)), ty)
transEXPR _ _ (BINOP op x y, ty) = do
  x' <- getVar x
  y' <- getVar y
  return (BINOP op x' y', ty)
transEXPR fn label (IFRET name t e, ty) = do
  e' <- transEXPR fn label e
  name' <- getVar name
  return (IFRET name' t e', ty)
transEXPR _ _ (RET name t, ty) = do
  name' <- getVar name
  return (RET name' t, ty)
transEXPR fn _ (IF ret c t f, ty) = do
  t' <- transBLOCK fn t
  f' <- transBLOCK fn f
  ret' <- getVar ret
  c' <- getVar c
  return (IF ret' c' t' f', ty)
transEXPR _ _ x = return x
