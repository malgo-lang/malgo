import           Test.Hspec

import           Control.Monad.State
import qualified Language.Malgo.Parser as P
import qualified Language.Malgo.Syntax as S
import qualified Language.Malgo.Typing as T

spec = do
  describe "Parse test" $ do
    it "def answer:Int = 42" $ do
      P.parse "def answer:Int = 42" `shouldBe` Right [S.Def (S.Sym "answer") S.IntTy (S.Int 42)]
    it "def not(x:Bool):Bool = if x #f else #t" $ do
      P.parse "def not(x:Bool):Bool = if x #f else #t" `shouldBe` Right [S.Defun (S.Sym "not") S.BoolTy [(S.Sym "x",S.BoolTy)] (S.If (S.Var (S.Sym "x")) (S.Bool False) (S.Bool True))]
    it "def prints():Unit = { print(\"hoge\"); print(\"foobar\") }" $ do
      P.parse "def prints():Unit = { print(\"hoge\"); print(\"foobar\") }" `shouldBe` Right [S.Defun (S.Sym "prints") S.UnitTy [] (S.Seq (S.Call (S.Sym "print") [S.String "hoge"]) (S.Call (S.Sym "print") [S.String "foobar"]))]
    it "def fib(n:Int):Int = if or(eq(n, 0), eq(n, 1)) 1 else fib(n-1) + fib(n-2)" $ do
      P.parse "def fib(n:Int):Int = if or(eq(n, 0), eq(n, 1)) 1 else fib(n-1) + fib(n-2)" `shouldBe`
        Right [S.Defun (S.Sym "fib") S.IntTy [(S.Sym "n", S.IntTy)]
               (S.If (S.Call (S.Sym "or") [ S.Call (S.Sym "eq") [S.Var (S.Sym "n"), S.Int 0]
                                  , S.Call (S.Sym "eq") [S.Var (S.Sym "n"), S.Int 1]])
                (S.Int 1) (S.Add (S.Call (S.Sym "fib") [S.Sub (S.Var (S.Sym "n")) (S.Int 1)])
                            (S.Call (S.Sym "fib") [S.Sub (S.Var (S.Sym "n")) (S.Int 2)])))]
    it "def lezero(n:Int):Bool = if n <= 0 #t else #f" $ do
      P.parse "def lezero(n:Int):Bool = if n <= 0 #t else #f" `shouldBe`
        Right [S.Defun (S.Sym "lezero") S.BoolTy [((S.Sym "n"), S.IntTy)]
               (S.If (S.Or (S.Lt (S.Var (S.Sym "n")) (S.Int 0)) (S.Eq (S.Var (S.Sym "n")) (S.Int 0)))
                (S.Bool True)
                (S.Bool False))]

  describe "Typing test(Expr)" $ do
    it "42" $ T.evalTypeofExpr (S.Int 42) `shouldBe` Right S.IntTy
    it "3.14" $ T.evalTypeofExpr (S.Float 3.14) `shouldBe` Right S.FloatTy
    it "#t" $ T.evalTypeofExpr (S.Bool True) `shouldBe` Right S.BoolTy
    it "'c'" $ T.evalTypeofExpr (S.Char 'c') `shouldBe` Right S.CharTy
    it "\"nyaaan\"" $ T.evalTypeofExpr (S.String "nyaaan") `shouldBe` Right S.StringTy
    it "unit" $ T.evalTypeofExpr S.Unit `shouldBe` Right S.UnitTy
    it "print(\"hello\")" $ T.evalTypeofExpr (S.Call (S.Sym "print") [S.String "hello"]) `shouldBe`
      Right S.UnitTy
    it "unit;1" $ T.evalTypeofExpr (S.Seq S.Unit (S.Int 1)) `shouldBe`
      Right S.IntTy
    it "if #t 1 else 2" $ T.evalTypeofExpr (S.If (S.Bool True) (S.Int 1) (S.Int 2)) `shouldBe`
      Right S.IntTy

    it "1 + 2" $ T.evalTypeofExpr (S.Add (S.Int 1) (S.Int 2)) `shouldBe`
      Right S.IntTy
    it "1 - 2" $ T.evalTypeofExpr (S.Sub (S.Int 1) (S.Int 2)) `shouldBe`
      Right S.IntTy
    it "1 * 2" $ T.evalTypeofExpr (S.Mul (S.Int 1) (S.Int 2)) `shouldBe`
      Right S.IntTy
    it "1 / 2" $ T.evalTypeofExpr (S.Div (S.Int 1) (S.Int 2)) `shouldBe`
      Right S.IntTy

    it "1.0 + 2.0" $ T.evalTypeofExpr (S.Add (S.Float 1) (S.Float 2)) `shouldBe`
      Right S.FloatTy
    it "1.0 - 2.0" $ T.evalTypeofExpr (S.Sub (S.Float 1) (S.Float 2)) `shouldBe`
      Right S.FloatTy
    it "1.0 * 2.0" $ T.evalTypeofExpr (S.Mul (S.Float 1) (S.Float 2)) `shouldBe`
      Right S.FloatTy
    it "1.0 / 2.0" $ T.evalTypeofExpr (S.Div (S.Float 1) (S.Float 2)) `shouldBe`
      Right S.FloatTy

  describe "Typing test(Decl)" $ do
    it "def a:Int = 42" $ do
      T.evalTypeofDecl (S.Def (S.Sym "a") S.IntTy (S.Int 42)) `shouldBe` Right S.IntTy
    it "def f(x:Int, y:Int):Int = x + y" $ do
      T.evalTypeofDecl (S.Defun (S.Sym "f") S.IntTy [((S.Sym "x"), S.IntTy), ((S.Sym "y"), S.IntTy)] (S.Add (S.Var (S.Sym "x")) (S.Var (S.Sym "y")))) `shouldBe`
        Right (S.FunTy S.IntTy [S.IntTy, S.IntTy])
    it "def g(x:Int):Int = if #t 0 else g(x-1)" $ do
      T.evalTypeofDecl (S.Defun (S.Sym "g") S.IntTy [((S.Sym "x"), S.IntTy)] (S.If (S.Bool True) (S.Int 0) (S.Call (S.Sym "g") [S.Sub (S.Var (S.Sym "x")) (S.Int 1)]))) `shouldBe`
        Right (S.FunTy S.IntTy [S.IntTy])
    it "def lezero(n:Int):Bool = if n <= 0 #t else #f" $ do
      T.evalTypeofDecl (S.Defun (S.Sym "lezero") S.BoolTy [((S.Sym "n"), S.IntTy)]
                        (S.If (S.Or (S.Lt (S.Var (S.Sym "n")) (S.Int 0)) (S.Eq (S.Var (S.Sym "n")) (S.Int 0)))
                         (S.Bool True)
                         (S.Bool False))) `shouldBe`
        Right (S.FunTy S.BoolTy [S.IntTy])

main :: IO ()
main = hspec spec
