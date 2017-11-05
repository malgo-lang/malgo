module Language.Malgo.ParserSpec (spec) where

import           Language.Malgo.Parser (parse)
import           Language.Malgo.Syntax
import           Language.Malgo.Types

import           Test.Hspec

spec :: Spec
spec = do
  describe "DefVar" $ do
    it "def a:Int = 42" $
      parse "<test>" "def a:Int = 42"
      `shouldBe`
      Right [DefVar dummyInfo (Name "a") IntTy (Int dummyInfo 42)]
    it "def b:Float = 3.14" $
      parse "<test>" "def b:Float = 3.14"
      `shouldBe`
      Right [DefVar dummyInfo (Name "b") FloatTy (Float dummyInfo 3.14)]
