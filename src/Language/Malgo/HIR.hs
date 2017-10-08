{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
module Language.Malgo.HIR where

import           Control.Arrow         ((&&&))
import           Control.Monad.State   ()
import           Language.Malgo.Syntax

data Id = Sym String
        | Tmp Int
  deriving (Eq, Show)

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
          | CALL Id [EXPR]
          | SEQ EXPR EXPR
          | BLOCK [EXPR]
          | LET Id Type EXPR
          | IF EXPR EXPR EXPR
          | BINOP Op EXPR EXPR
  deriving (Eq, Show)

newtype HIR (a :: Phase) = HIR { unHIR :: DECL }
  deriving (Eq, Show)

data Phase = Raw | KNormal | Alpha
  deriving (Eq, Show)

name2Id :: Name -> Id
name2Id = Sym

transExpr :: Expr -> EXPR
transExpr (Var _ name)        = VAR (name2Id name)
transExpr (Int _ i)           = INT i
transExpr (Float _ f)         = FLOAT f
transExpr (Bool _ b)          = BOOL b
transExpr (Char _ c)          = CHAR c
transExpr (String _ s)        = STRING s
transExpr (Unit _)            = UNIT
transExpr (Call _ name xs)    = CALL (name2Id name) (map transExpr xs)
transExpr e@Seq{}             = BLOCK $ transSeq e
transExpr (Let _ name ty val) = LET (name2Id name) ty (transExpr val)
transExpr (If _ c t f)        = IF (transExpr c) (transExpr t) (transExpr f)
transExpr (BinOp _ op e1 e2)  = BINOP op (transExpr e1) (transExpr e2)

transSeq :: Expr -> [EXPR]
transSeq (Seq _ e1 e2) = transExpr e1 : transSeq e2
transSeq x             = [transExpr x]

transDecl :: Decl -> DECL
transDecl (Def _ name ty val)              = DEF (name2Id name) ty (transExpr val)
transDecl (Defun _ name retty params body) = DEFUN (name2Id name) retty (map ((name2Id . fst) &&& snd) params) (transExpr body)
transDecl (ExDef _ name ty) = EXDEF (name2Id name) ty
transDecl (ExDefun _ name retty params) = EXDEFUN (name2Id name) retty (map ((name2Id . fst) &&& snd) params)

trans :: Decl -> HIR 'Raw
trans decl = HIR $ transDecl decl
