module Malgo.ParserTest
  ( unit_var,
    unit_lit,
    unit_apply,
    unit_this,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Malgo.Parser
import Malgo.Prelude
import Malgo.Syntax
import Test.Tasty.HUnit (Assertion, (@?=))

unit_var :: Assertion
unit_var = parse "test.malgo" "x" @?= Right (Var "x")

unit_lit :: Assertion
unit_lit = do
  parse "test.malgo" "42" @?= Right (Lit $ Int 42)
  parse "test.malgo" "0" @?= Right (Lit $ Int 0)

unit_apply :: Assertion
unit_apply = do
  parse "test.malgo" "f x" @?= Right (App (Var "f") (Var "x" :| []))
  parse "test.malgo" "f x y" @?= Right (App (Var "f") (Var "x" :| [Var "y"]))
  parse "test.malgo" "f x y z" @?= Right (App (Var "f") (Var "x" :| [Var "y", Var "z"]))

unit_this :: Assertion
unit_this = do
  parse "test.malgo" "#" @?= Right (Var "#")