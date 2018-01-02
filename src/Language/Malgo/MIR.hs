module Language.Malgo.MIR
  ( Decl(..)
  , Expr(..)
  , Instr(..)
  ) where

import           Language.Malgo.HIR    (Op (..))
import qualified Language.Malgo.HIR    as H
import           Language.Malgo.Syntax (Type (..))
import qualified Language.Malgo.Syntax as S
import           Language.Malgo.Utils
import           Text.PrettyPrint

data Expr a = Int Integer
            | Float Double
            | Bool Bool
            | Char Char
            --- | String String
            | Unit
            | CallDir { _fnName :: Name
                      , _args   :: [a]
                      }
            | CallCls { _fn   :: a
                      , _args :: [a]
                      , _fv   :: [a]
                      }
            | If a (Expr a) (Expr a)
            | BinOp Op a a
            deriving (Show)

data Instr a = a := (Expr a)
             | Do (Expr a)
             deriving Show

data Decl a = FunDec { _name    :: a
                     , _params  :: [a]
                     , _capture :: [a]
                     , _body    :: [Instr a]
                     }
            | StrDec { _name :: a
                     , _val  :: String
                     }
            | ExDec { _name   :: a
                    , _actual :: String
                    }
            deriving Show

data Program a = Program { _toplevel :: [Decl a]
                         , _main     :: [Instr a]
                         }
  deriving Show
