module Language.Malgo.IR where

import           Control.Monad.State
import           Language.Malgo.Syntax

data DECL = DEF Name Type EXPR
          | DEFUN Name Type [(Name, Type)] EXPR
  deriving (Eq, Show)

data EXPR = VAR Name
          | INT Int
          | FLOAT Double
          | BOOL Bool
          | CHAR Char
          | STRING String
          | UNIT
          | CALL Name [EXPR]
          | SEQ EXPR EXPR
          | BLOCK [EXPR]
          | LET Name Type EXPR
          | IF EXPR EXPR EXPR
          | BINOP Op EXPR EXPR
  deriving (Eq, Show)

newtype IR = IR { unIR :: (DECL, Phase) }
  deriving (Eq, Show)

data Phase = Raw | Alpha | KNormal
  deriving (Eq, Show)

transExpr :: Expr -> EXPR
transExpr (Var _ name)        = VAR name
transExpr (Int _ i)           = INT i
transExpr (Float _ f)         = FLOAT f
transExpr (Bool _ b)          = BOOL b
transExpr (Char _ c)          = CHAR c
transExpr (String _ s)        = STRING s
transExpr (Unit _)            = UNIT
transExpr (Call _ name xs)    = CALL name (map transExpr xs)
-- transExpr (Seq _ e1 e2)       = SEQ (transExpr e1) (transExpr e2)
transExpr e@Seq{}             = BLOCK $ transSeq e
transExpr (Let _ name ty val) = LET name ty (transExpr val)
transExpr (If _ c t f)        = IF (transExpr c) (transExpr t) (transExpr f)
transExpr (BinOp _ op e1 e2)  = BINOP op (transExpr e1) (transExpr e2)

transSeq :: Expr -> [EXPR]
transSeq (Seq _ e1 e2) = transExpr e1 : transSeq e2
transSeq x             = [transExpr x]

transDecl :: Decl -> DECL
transDecl (Def _ name ty val)              = DEF name ty (transExpr val)
transDecl (Defun _ name retty params body) = DEFUN name retty params (transExpr body)

trans :: Decl -> IR
trans decl = IR (transDecl decl, Raw)
