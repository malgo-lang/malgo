{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
module Language.Malgo.HIR where

import           Language.Malgo.Syntax (Name, Op, Type (..))
-- import           Text.PrettyPrint

newtype Id = Sym String
  deriving (Eq, Show)

newtype HIR (a :: Phase) = HIR { unHIR :: DECL a }
  deriving (Eq, Show)

data Phase = Typed | KNormal
  deriving (Eq, Show)

data DECL (a :: Phase) = DEF Id Type (EXPR a)
                       | DEFUN Id Type [(Id, Type)] (EXPR a)
                       | EXDEF Id Type
                       | EXDEFUN Id Type [(Id, Type)]
  deriving (Eq, Show)

data EXPR' (a :: Phase) = VAR Id
                        | INT Int
                        | FLOAT Double
                        | BOOL Bool
                        | CHAR Char
                        | STRING String
                        | UNIT
                        | CALL { _callName :: Id
                               , _callArgs :: [EXPR a]
                               }
                        | LET { _letName  :: Id
                              , _letType  :: Type
                              , _letValue :: EXPR a
                              , _letBody  :: EXPR a}
                        | IF (EXPR a) (EXPR a) (EXPR a)
                        | BINOP Op (EXPR a) (EXPR a)
  deriving (Eq, Show)

type EXPR (a :: Phase) = (EXPR' a, Type)

name2Id :: Name -> Id
name2Id = Sym
