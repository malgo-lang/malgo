{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Malgo.MIR where

import           Control.Lens
import           Control.Monad.State
import           Language.Malgo.HIR    (Id (..))
import qualified Language.Malgo.HIR    as H
import           Language.Malgo.Syntax (Op, Type)

data DECL = DEF Id Type EXPR
          | DEFUN Id Type [(Id, Type)] BLOCK
          | EXDEF Id Type
          | EXDEFUN Id Type [(Id, Type)]
  deriving (Eq, Show)

data BLOCK = BLOCK Id [EXPR]
  deriving (Eq, Show)

data EXPR = VAR Id
          | INT Int
          | FLOAT Double
          | BOOL Bool
          | CHAR Char
          | STRING String
          | UNIT
          | CALL Id [Id]
          | LET Id Type EXPR
          | IF Id BLOCK BLOCK
          | BINOP Op Id Id
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
transBLOCK' (H.LET name typ val body, _) = (:) <$> (LET name typ <$> transEXPR val) <*> transBLOCK' body
transBLOCK' e = (:[]) <$> transEXPR e

transEXPR :: H.EXPR 'H.KNormal -> State Env EXPR
transEXPR (H.VAR name, _) = return $ VAR name
transEXPR (H.INT x, _)    = return $ INT x
transEXPR (H.FLOAT x, _)  = return $ FLOAT x
transEXPR (H.BOOL x, _)   = return $ BOOL x
transEXPR (H.CHAR x, _)   = return $ CHAR x
transEXPR (H.STRING x, _) = return $ STRING x
transEXPR (H.UNIT, _) = return UNIT
transEXPR (H.CALL fn args, _) =
  return $ CALL fn (map
                    (\case
                        (H.VAR x, _) -> x
                        _ -> error $ "HIR -> MIR error: args = " ++ show args)
                    args)
transEXPR (e@H.LET{}, _) = error $ "HIR -> MIR error: LET cannot be EXPR " ++ show e
  -- LET name typ <$> transEXPR val <*> transBLOCK body
transEXPR (H.IF (H.VAR c, _) t f, _) = do
  tlabel <- newLabel "then"
  flabel <- newLabel "else"
  IF c <$> transBLOCK tlabel t <*> transBLOCK flabel f
transEXPR (e@H.IF{}, _) = error $ "HIR -> MIR error: IF c t f = " ++ show e
transEXPR (H.BINOP op (H.VAR x, _) (H.VAR y, _), _) = return $ BINOP op x y
transEXPR (e@H.BINOP{}, _) = error $ "HIR -> MIR error: BINOP op x y = " ++ show e
