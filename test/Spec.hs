import           Test.Hspec

import qualified Language.Malgo.Parser as MP
import qualified Language.Malgo.Syntax as MS
import qualified Text.Parsec           as P
import qualified Text.Parsec.String    as P

spec = do
  describe "textAST" $ do
    it "sample1" $ do
      MS.textAST MS.sample1 `shouldBe` "(def ans:Int 42)"
  describe "parse" $ do
    it "sample1" $ do
      P.parse MP.parseExpr "" (MS.textAST MS.sample1) `shouldBe` Right MS.sample1
    it "sample2" $ do
      P.parse MP.parseExpr "" (MS.textAST MS.sample2) `shouldBe` Right MS.sample2
    it "sample3" $ do
      P.parse MP.parseExpr "" (MS.textAST MS.sample3) `shouldBe` Right MS.sample3
    it "sample4" $ do
      P.parse MP.parseExpr "" (MS.textAST MS.sample4) `shouldBe` Right MS.sample4
    it "hoge:Hoge" $ do
      let ast = MS.Typed (MS.Symbol "hoge") (MS.Symbol "Hoge")
      P.parse MP.parseExpr "" (MS.textAST ast) `shouldBe` Right ast
    it "Fn" $ do
      let src = "(def (f:int g:(fn i8 int) x:i8) (g x))"
      P.parse MP.parseExpr "" src `shouldBe` (Right $ MS.Tree [ MS.Symbol "def"
                                                              , MS.Tree [ MS.Typed (MS.Symbol "f") (MS.Symbol "int")
                                                                        , MS.Typed (MS.Symbol "g")
                                                                          (MS.Tree [MS.Symbol "fn", MS.Symbol "i8", MS.Symbol "int"])
                                                                        , MS.Typed (MS.Symbol "x") (MS.Symbol "i8")]
                                                              , MS.Tree [MS.Symbol "g", MS.Symbol "x"]
                                                              ])
    it "String" $ do
      let src = "(print \"hoge\")"
      P.parse MP.parseExpr "" src `shouldBe` Right (MS.Tree [MS.Symbol "print", MS.String "hoge"])
    it "Char" $ do
      let src = "(print-char 'a')"
      P.parse MP.parseExpr "" src `shouldBe` Right (MS.Tree [MS.Symbol "print-char", MS.Char 'a'])

main :: IO ()
main = hspec spec
