{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}
module Language.Malgo.MIR where

import           Language.Malgo.HIR    (Id)
import qualified Language.Malgo.HIR    as H
import           Language.Malgo.Syntax (Op, Type)

data DECL = DEF Id Type EXPR
          | DEFUN Id Type [(Id, Type)] EXPR
          | EXDEF Id Type
          | EXDEFUN Id Type [(Id, Type)]
  deriving (Eq, Show)

data EXPR = VAR Id
          | INT Int
          | FLOAT Double
          | BOOL Bool
          | CHAR Char
          | STRING String
          | UNIT
          | CALL Id [Id]
          | LET Id Type EXPR EXPR
          | IF Id EXPR EXPR
          | BINOP Op Id Id
  deriving (Eq, Show)

trans :: H.HIR 'H.KNormal -> DECL
trans (H.HIR d) = transDECL d

transDECL :: H.DECL 'H.KNormal -> Language.Malgo.MIR.DECL
transDECL (H.DEF name typ val) = DEF name typ (transEXPR val)
transDECL (H.DEFUN fnName retTy params body) = DEFUN fnName retTy params (transEXPR body)
transDECL (H.EXDEF name typ) = EXDEF name typ
transDECL (H.EXDEFUN fnName retTy params) = EXDEFUN fnName retTy params

transEXPR :: H.EXPR 'H.KNormal -> Language.Malgo.MIR.EXPR
transEXPR (H.VAR name, _) = VAR name
transEXPR (H.INT x, _)    = INT x
transEXPR (H.FLOAT x, _)  = FLOAT x
transEXPR (H.BOOL x, _)   = BOOL x
transEXPR (H.CHAR x, _)   = CHAR x
transEXPR (H.STRING x, _) = STRING x
transEXPR (H.UNIT, _) = UNIT
transEXPR (H.CALL fn args, _) =
  CALL fn (map
           (\case
               (H.VAR x, _) -> x
               _ -> error $ "HIR -> MIR error: args = " ++ show args)
           args)
transEXPR (H.LET name typ val body, _) =
  LET name typ (transEXPR val) (transEXPR body)
transEXPR (H.IF (H.VAR c, _) t f, _) = IF c (transEXPR t) (transEXPR f)
transEXPR (e@H.IF{}, _) = error $ "HIR -> MIR error: IF c t f = " ++ show e
transEXPR (H.BINOP op (H.VAR x, _) (H.VAR y, _), _) = BINOP op x y
transEXPR (e@H.BINOP{}, _) = error $ "HIR -> MIR error: BINOP op x y = " ++ show e
