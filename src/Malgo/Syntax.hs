module Malgo.Syntax where

import Malgo.Location (Location)
import Malgo.Prelude

data Term a
  = Var Location a
  | Literal Location Literal
  | Construct Location Text [Term a] [Term a]
  | Comatch Location [(Copattern a, Term a)]
  | Destruct Location (Term a) Text [Term a] [Term a]
  | Match Location (Term a) [(Pattern a, Term a)]
  | Prim Location Text [Term a] [Term a]
  | Switch Location (Term a) [(Literal, Term a)] (Term a)
  | Invoke Location a [Term a] [Term a]
  | Label Location a (Term a)
  | Goto Location (Term a) a

data Literal = Int Int

type Pattern a = (Text, [a], [a])

type Copattern a = (Text, [a], [a])