module Malgo.ParserSpec.BraceTupleSpec where

import Data.Text.Lazy qualified as TL
import Malgo.Monad (runMalgoM)
import Malgo.Parser (parse)
import Malgo.Prelude
import Malgo.Syntax
import Malgo.TestUtils (flag)
import Test.Hspec

spec :: Spec
spec = describe "Brace Tuple Syntax" $ do
  it "parses {x, y} as a tuple" $ do
    result <- runMalgoM flag $ parse "test.mlg" "def x = {1L, 2L}"
    result `shouldSatisfy` isRight

  it "parses {x, y, z} as a triple" $ do
    result <- runMalgoM flag $ parse "test.mlg" "def x = {1L, 2L, 3L}"
    result `shouldSatisfy` isRight

  it "parses tuple patterns {x, y}" $ do
    result <- runMalgoM flag $ parse "test.mlg" "def f = { {x, y} -> x }"
    result `shouldSatisfy` isRight
