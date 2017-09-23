import           Test.Hspec

import qualified Language.Malgo.Parser as P
import qualified Language.Malgo.Syntax as S

spec = do
  -- describe "textAST" $ do
  --   it "sample1" $ do
  --     MS.textAST MS.sample1 `shouldBe` "(def ans:Int 42)"
  describe "Parse test" $ do
    it "def answer:Int = 42" $ do
      P.parse "def answer:Int = 42" `shouldBe` Right [S.Def "answer" S.IntTy (S.Int 42)]
    it "def not(x:Bool):Bool = if x #f else #t" $ do
      P.parse "def not(x:Bool):Bool = if x #f else #t" `shouldBe` Right [S.Defun "not" S.BoolTy [("x",S.BoolTy)] (S.If (S.Var "x") (S.Bool False) (S.Bool True))]
    it "def prints():Int = { print(\"hoge\"); print(\"foobar\") }" $ do
      P.parse "def prints():Int = { print(\"hoge\"); print(\"foobar\") }" `shouldBe` Right [S.Defun "prints" S.IntTy [] (S.Block [S.Call "print" [S.String "hoge"],S.Call "print" [S.String "foobar"]])]

main :: IO ()
main = hspec spec
