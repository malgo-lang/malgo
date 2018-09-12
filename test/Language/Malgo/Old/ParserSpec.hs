module Language.Malgo.Old.ParserSpec
    ( spec
    ) where

import Prelude
import           Language.Malgo.Old.Parser (parse)
import           Language.Malgo.Old.IR.Syntax

import           Test.Hspec

spec :: Spec
-- spec = do
--   describe "DefVar" $ do
--     it "var a:Int = 42" $
--       parse "<test>" "var a:Int = 42"
--       `shouldBe`
--       Right [DefVar dummyInfo (Name "a") IntTy (Int dummyInfo 42)]
--     it "var b:Float = 3.14" $
--       parse "<test>" "var b:Float = 3.14"
--       `shouldBe`
--       Right [DefVar dummyInfo (Name "b") FloatTy (Float dummyInfo 3.14)]
spec = return ()
