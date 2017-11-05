{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Malgo.MIR where

import           Control.Lens
import           Control.Monad.State
import           Language.Malgo.HIR    (Id (..))
import qualified Language.Malgo.HIR    as H
import           Language.Malgo.Syntax (Op, Type (..))

data DECL = DEF Id Type EXPR
          | DEFUN Id Type [(Id, Type)] BLOCK
          | EXDEF Id Type
          | EXDEFUN Id Type [(Id, Type)]
  deriving (Eq, Show)

data BLOCK = BLOCK Id [EXPR]
  deriving (Eq, Show)

type EXPR = (EXPR', Type)

data EXPR' = VAR Id
           | INT Int
           | FLOAT Double
           | BOOL Bool
           | CHAR Char
           | STRING String
           | UNIT
           | NOP
           | CALL (Id, Type) [(Id, Type)]
           | LET Id Type EXPR
           | IF Id Id BLOCK BLOCK
           | BINOP Op Id Id
           | IFRET Id Type EXPR
           | RET Id Type
  deriving (Eq, Show)

newtype Env = Env { _labelCount :: Int }
  deriving (Eq, Show)
makeLenses ''Env

newLabel :: String -> State Env Id
newLabel hint = do
  c <- use labelCount
  labelCount .= c + 1
  return $ Sym ("$B" ++ show c ++ "_" ++ hint)

trans :: H.HIR 'H.KNormal -> (DECL, Env)
trans (H.HIR d) = runState (transDECL d) (Env 0)

transDECL :: H.DECL 'H.KNormal -> State Env DECL
transDECL (H.DEF name typ val) = DEF name typ <$> transEXPR val
transDECL (H.DEFUN fnName retTy params body) = DEFUN fnName retTy params <$> transBLOCK (Sym "$B_entry") body
transDECL (H.EXDEF name typ) = return $ EXDEF name typ
transDECL (H.EXDEFUN fnName retTy params) = return $ EXDEFUN fnName retTy params

transBLOCK :: Id -> H.EXPR 'H.KNormal -> State Env BLOCK
transBLOCK name hexpr = BLOCK name <$> transBLOCK' hexpr

transBLOCK' :: H.EXPR 'H.KNormal -> State Env [EXPR]
transBLOCK' (H.LET name typ val body, _) = do
  letExpr' <- LET name typ <$> transEXPR val
  (:) <$> pure (letExpr', UnitTy) <*> transBLOCK' body
transBLOCK' (H.UNIT, _) = return []
transBLOCK' e = (:[]) <$> transEXPR e

-- insertRET :: BLOCK -> BLOCK
-- insertRET (BLOCK i es) = BLOCK i $ insertRET' es
--   where insertRET' [(IF ret c t f, ty)] = [(IF ret c (insertRET t) (insertRET f), ty)]
--         insertRET' [e]              = [(RET e, UnitTy)]
--         insertRET' (e:es')          = e : insertRET' es'
--         insertRET' []               = undefined

transEXPR :: H.EXPR 'H.KNormal -> State Env EXPR
transEXPR (H.VAR name, t) = return (VAR name, t)
transEXPR (H.INT x, t)    = return (INT x, t)
transEXPR (H.FLOAT x, t)  = return (FLOAT x, t)
transEXPR (H.BOOL x, t)   = return (BOOL x, t)
transEXPR (H.CHAR x, t)   = return (CHAR x, t)
transEXPR (H.STRING x, t) = return (STRING x, t)
transEXPR (H.UNIT, t) = return (UNIT, t)
transEXPR (H.CALL fn args, t) =
  return (CALL
          (fn, FunTy t (map snd args))
          (map
            (\case
                (H.VAR x, xt) -> (x, xt)
                _ -> error $ "HIR -> MIR error: args = " ++ show args)
            args), t)
transEXPR (e@H.LET{}, _) = error $ "HIR -> MIR error: LET cannot be EXPR " ++ show e
  -- LET name typ <$> transEXPR val <*> transBLOCK body
transEXPR (H.IF (H.VAR c, _) t f, ty) = do
  tlabel <- newLabel "then"
  flabel <- newLabel "else"
  if' <- IF (Sym "") c <$> transBLOCK tlabel t <*> transBLOCK flabel f
  return (if', ty)
transEXPR (e@H.IF{}, _) = error $ "HIR -> MIR error: IF c t f = " ++ show e
transEXPR (H.BINOP op (H.VAR x, _) (H.VAR y, _), t) = return (BINOP op x y, t)
transEXPR (e@H.BINOP{}, _) = error $ "HIR -> MIR error: BINOP op x y = " ++ show e
