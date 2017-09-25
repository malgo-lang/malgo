import           Test.Hspec

import qualified Language.Malgo.Parser as P
import qualified Language.Malgo.Syntax as S

spec = do
  describe "Parse test" $ do
    it "def answer:Int = 42" $ do
      P.parse "def answer:Int = 42" `shouldBe` Right [S.Def "answer" S.IntTy (S.Int 42)]
    it "def not(x:Bool):Bool = if x #f else #t" $ do
      P.parse "def not(x:Bool):Bool = if x #f else #t" `shouldBe` Right [S.Defun "not" S.BoolTy [("x",S.BoolTy)] (S.If (S.Var "x") (S.Bool False) (S.Bool True))]
    it "def prints():Unit = { print(\"hoge\"); print(\"foobar\") }" $ do
      P.parse "def prints():Unit = { print(\"hoge\"); print(\"foobar\") }" `shouldBe` Right [S.Defun "prints" S.UnitTy [] (S.Seq (S.Call "print" [S.String "hoge"]) (S.Call "print" [S.String "foobar"]))]
    it "def fib(n:Int):Int = if or(eq(n, 0), eq(n, 1)) 1 else fib(n-1) + fib(n-2)" $ do
      P.parse "def fib(n:Int):Int = if or(eq(n, 0), eq(n, 1)) 1 else fib(n-1) + fib(n-2)" `shouldBe`
        Right [S.Defun "fib" S.IntTy [("n", S.IntTy)]
               (S.If (S.Call "or" [ S.Call "eq" [S.Var "n", S.Int 0]
                                  , S.Call "eq" [S.Var "n", S.Int 1]])
                (S.Int 1) (S.Add (S.Call "fib" [S.Sub (S.Var "n") (S.Int 1)])
                            (S.Call "fib" [S.Sub (S.Var "n") (S.Int 2)])))]

main :: IO ()
main = hspec spec
