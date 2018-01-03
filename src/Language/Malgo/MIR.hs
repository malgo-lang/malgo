module Language.Malgo.MIR
  ( Decl(..)
  , Expr(..)
  , Instr(..)
  , Program(..)
  ) where

import           Language.Malgo.HIR   (Op (..))
import           Language.Malgo.Utils

data Expr a = Var a
            | Int Integer
            | Float Double
            | Bool Bool
            | Char Char
            | Unit
            | MakeCls a [a]
            | CallDir { _fnName :: Name
                      , _args   :: [a]
                      }
            | CallCls { _cls  :: a
                      , _args :: [a]
                      }
            | If a (Expr a) (Expr a)
            | BinOp Op a a
            deriving (Show)

data Instr a = a := (Expr a)
             | Do (Expr a)
             deriving Show

data Decl a = FunDec { _decName :: a
                     , _params  :: [a]
                     , _capture :: [a]
                     , _body    :: [Instr a]
                     }
            | StrDec { _decName :: a
                     , _val     :: String
                     }
            | ExDec { _decName :: a
                    , _actual  :: String
                    }
            deriving Show

data Program a = Program { _toplevel :: [Decl a]
                         , _main     :: [Instr a]
                         }
  deriving Show
