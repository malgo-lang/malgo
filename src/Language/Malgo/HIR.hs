{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
module Language.Malgo.HIR where

import           Language.Malgo.Syntax

data Id = Sym { id2name :: String}
        | Tmp Int
  deriving (Eq, Show)

data DECL = DEF Id Type EXPR
          | DEFUN Id Type [(Id, Type)] EXPR
          | EXDEF Id Type
          | EXDEFUN Id Type [(Id, Type)]
  deriving (Eq, Show)

data EXPR' = VAR Id
           | INT Int
           | FLOAT Double
           | BOOL Bool
           | CHAR Char
           | STRING String
           | UNIT
           | CALL Id [EXPR]
           | SEQ EXPR EXPR
           -- | BLOCK { unBLOCK :: [EXPR] }
           | LET Id Type EXPR
           | IF EXPR EXPR EXPR
           | BINOP Op EXPR EXPR
  deriving (Eq, Show)

type EXPR = (EXPR', Type)

newtype HIR (a :: Phase) = HIR { unHIR :: DECL }
  deriving (Eq, Show)

data Phase = Typed | KNormal | Alpha
  deriving (Eq, Show)

name2Id :: Name -> Id
name2Id = Sym

-- transExpr :: Expr -> EXPR
-- transExpr (Var _ name)        = VAR (name2Id name)
-- transExpr (Int _ i)           = INT i
-- transExpr (Float _ f)         = FLOAT f
-- transExpr (Bool _ b)          = BOOL b
-- transExpr (Char _ c)          = CHAR c
-- transExpr (String _ s)        = STRING s
-- transExpr (Unit _)            = UNIT
-- transExpr (Call _ name xs)    = CALL (name2Id name) (map transExpr xs)
-- transExpr e@Seq{}             = transSeq e
-- transExpr (Let _ name ty val) = LET (name2Id name) ty (transExpr val) [UNIT]
-- transExpr (If _ c t f)        = IF (transExpr c) (transExpr t) (transExpr f)
-- transExpr (BinOp _ op e1 e2)  = BINOP op (transExpr e1) (transExpr e2)

-- transSeq :: Expr -> EXPR
-- transSeq (Seq _ (Let _ name ty val) e) =
--   LET (name2Id name) ty (transExpr val) $ case transExpr e of
--                                   BLOCK xs -> xs
--                                   x        -> [x]
-- transSeq (Seq _ e1 e2) = BLOCK $ transExpr e1 : unBLOCK (transSeq e2)
-- transSeq x             = BLOCK [transExpr x]
