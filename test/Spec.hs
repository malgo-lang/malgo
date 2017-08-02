import           Test.Hspec

import qualified Language.Malgo.Syntax as MS
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified Language.Malgo.Parser as MP

spec = do
  describe "textAST" $ do
    it "sample1" $ do
      MS.textAST MS.sample1 `shouldBe` "(def ans:Int 42)"
  describe "parse" $ do
    it "sample1" $ do
      print MS.sample1
      print (MS.textAST MS.sample1)
      P.parse MP.parseExpr "" (MS.textAST MS.sample1) `shouldBe` Right MS.sample1
    it "sample2" $ do
      print MS.sample2
      print (MS.textAST MS.sample2)
      P.parse MP.parseExpr "" (MS.textAST MS.sample2) `shouldBe` Right MS.sample2
    it "sample3" $ do
      print MS.sample3
      print (MS.textAST MS.sample3)
      P.parse MP.parseExpr "" (MS.textAST MS.sample3) `shouldBe` Right MS.sample3
    it "sample4" $ do
      print MS.sample4
      print (MS.textAST MS.sample4)
      P.parse MP.parseExpr "" (MS.textAST MS.sample4) `shouldBe` Right MS.sample4

main :: IO ()
main = hspec spec
