import           Test.Hspec

-- import           Control.Monad.State
import qualified Language.Malgo.Parser as P
import qualified Language.Malgo.Syntax as S
import qualified Language.Malgo.Typing as T

spec = do
  describe "Parse test" $ do
    it "def answer:Int = 42" $
      P.parse "def answer:Int = 42" `shouldBe`
        Right [S.Def (S.mkName "answer") S.IntTy (S.Int 42)]

    it "def pi:Float = 3.14" $ do
      P.parse "def pi:Float = 3.14" `shouldBe` Right [S.Def (S.mkName "pi") S.FloatTy (S.Float 3.14)]

    it "def not(x:Bool):Bool = if x #f else #t" $ do
      P.parse "def not(x:Bool):Bool = if x #f else #t" `shouldBe` Right [S.Defun (S.mkName "not") S.BoolTy [(S.mkName "x",S.BoolTy)] (S.If (S.Var (S.mkName "x")) (S.Bool False) (S.Bool True))]

    it "def prints():Unit = { print(\"hoge\"); print(\"foobar\") }" $ do
      P.parse "def prints():Unit = { print(\"hoge\"); print(\"foobar\") }" `shouldBe` Right [S.Defun (S.mkName "prints") S.UnitTy [] (S.Seq (S.Call (S.mkName "print") [S.String "hoge"]) (S.Call (S.mkName "print") [S.String "foobar"]))]

    it "def fib(n:Int):Int = if or(eq(n, 0), eq(n, 1)) 1 else fib(n-1) + fib(n-2)" $ do
      P.parse "def fib(n:Int):Int = if or(eq(n, 0), eq(n, 1)) 1 else fib(n-1) + fib(n-2)" `shouldBe`
        Right [S.Defun (S.mkName "fib") S.IntTy [(S.mkName "n", S.IntTy)]
               (S.If (S.Call (S.mkName "or") [ S.Call (S.mkName "eq") [S.Var (S.mkName "n"), S.Int 0]
                                  , S.Call (S.mkName "eq") [S.Var (S.mkName "n"), S.Int 1]])
                (S.Int 1) (S.BinOp S.Add
                            (S.Call (S.mkName "fib") [S.BinOp S.Sub (S.Var (S.mkName "n")) (S.Int 1)])
                            (S.Call (S.mkName "fib") [S.BinOp S.Sub (S.Var (S.mkName "n")) (S.Int 2)])))]

    it "def lezero(n:Int):Bool = if n <= 0 #t else #f" $ do
      P.parse "def lezero(n:Int):Bool = if n <= 0 #t else #f" `shouldBe`
        Right [S.Defun (S.mkName "lezero") S.BoolTy [((S.mkName "n"), S.IntTy)]
               (S.If (S.BinOp S.Le
                       (S.Var (S.mkName "n"))
                       (S.Int 0))
                (S.Bool True)
                (S.Bool False))]

    it "def answer:Int = let a:Int = 6 * 7; a" $ do
      P.parse "def answer:Int = let a:Int = 6 * 7; a" `shouldBe`
        Right [S.Def (S.mkName "answer") S.IntTy (S.Seq (S.Let (S.mkName "a") S.IntTy (S.BinOp S.Mul (S.Int 6) (S.Int 7)))
                                                   (S.Var (S.mkName "a")))]

  describe "Typing test(Expr)" $ do
    it "42" $ T.evalTypeofExpr (S.Int 42) `shouldBe` Right S.IntTy
    it "3.14" $ T.evalTypeofExpr (S.Float 3.14) `shouldBe` Right S.FloatTy
    it "#t" $ T.evalTypeofExpr (S.Bool True) `shouldBe` Right S.BoolTy
    it "'c'" $ T.evalTypeofExpr (S.Char 'c') `shouldBe` Right S.CharTy
    it "\"nyaaan\"" $ T.evalTypeofExpr (S.String "nyaaan") `shouldBe` Right S.StringTy
    it "unit" $ T.evalTypeofExpr S.Unit `shouldBe` Right S.UnitTy
    it "print(\"hello\")" $ T.evalTypeofExpr (S.Call (S.mkName "print") [S.String "hello"]) `shouldBe`
      Right S.UnitTy
    it "unit;1" $ T.evalTypeofExpr (S.Seq S.Unit (S.Int 1)) `shouldBe`
      Right S.IntTy
    it "if #t 1 else 2" $ T.evalTypeofExpr (S.If (S.Bool True) (S.Int 1) (S.Int 2)) `shouldBe`
      Right S.IntTy

    it "let a:Int = 1" $ T.evalTypeofExpr (S.Let (S.mkName "a") S.IntTy (S.Int 1)) `shouldBe`
      Right S.UnitTy

    it "let a:Int = 1; a" $ T.evalTypeofExpr (S.Seq (S.Let (S.mkName "a") S.IntTy (S.Int 1)) (S.Var (S.mkName "a"))) `shouldBe`
      Right S.IntTy

    it "1 + 2" $ T.evalTypeofExpr (S.BinOp S.Add (S.Int 1) (S.Int 2)) `shouldBe`
      Right S.IntTy
    it "1 - 2" $ T.evalTypeofExpr (S.BinOp S.Sub (S.Int 1) (S.Int 2)) `shouldBe`
      Right S.IntTy
    it "1 * 2" $ T.evalTypeofExpr (S.BinOp S.Mul (S.Int 1) (S.Int 2)) `shouldBe`
      Right S.IntTy
    it "1 / 2" $ T.evalTypeofExpr (S.BinOp S.Div (S.Int 1) (S.Int 2)) `shouldBe`
      Right S.IntTy

    it "1.0 + 2.0" $ T.evalTypeofExpr (S.BinOp S.Add (S.Float 1) (S.Float 2)) `shouldBe`
      Right S.FloatTy
    it "1.0 - 2.0" $ T.evalTypeofExpr (S.BinOp S.Sub (S.Float 1) (S.Float 2)) `shouldBe`
      Right S.FloatTy
    it "1.0 * 2.0" $ T.evalTypeofExpr (S.BinOp S.Mul (S.Float 1) (S.Float 2)) `shouldBe`
      Right S.FloatTy
    it "1.0 / 2.0" $ T.evalTypeofExpr (S.BinOp S.Div (S.Float 1) (S.Float 2)) `shouldBe`
      Right S.FloatTy

  describe "Typing test(Decl)" $ do
    it "def a:Int = 42" $ do
      T.evalTypeofDecl (S.Def (S.mkName "a") S.IntTy (S.Int 42)) `shouldBe` Right S.IntTy

    it "def f(x:Int, y:Int):Int = x + y" $ do
      T.evalTypeofDecl (S.Defun (S.mkName "f") S.IntTy [((S.mkName "x"), S.IntTy), ((S.mkName "y"), S.IntTy)] (S.BinOp S.Add (S.Var (S.mkName "x")) (S.Var (S.mkName "y")))) `shouldBe`
        Right (S.FunTy S.IntTy [S.IntTy, S.IntTy])

    it "def g(x:Int):Int = if #t 0 else g(x-1)" $ do
      T.evalTypeofDecl (S.Defun (S.mkName "g") S.IntTy [((S.mkName "x"), S.IntTy)]
                        (S.If (S.Bool True) (S.Int 0) (S.Call (S.mkName "g") [S.BinOp S.Sub (S.Var (S.mkName "x")) (S.Int 1)]))) `shouldBe`
        Right (S.FunTy S.IntTy [S.IntTy])

    it "def lezero(n:Int):Bool = if n <= 0 #t else #f" $ do
      T.evalTypeofDecl (S.Defun (S.mkName "lezero") S.BoolTy [((S.mkName "n"), S.IntTy)]
                        (S.If (S.BinOp S.Le (S.Var (S.mkName "n")) (S.Int 0))
                         (S.Bool True)
                         (S.Bool False))) `shouldBe`
        Right (S.FunTy S.BoolTy [S.IntTy])

  describe "Pretty Printer test" $ do
    it "def a:Int = 42" $ P.parse (show (S.prettyDecl (S.Def (S.mkName "a") S.IntTy (S.Int 42)))) `shouldBe`
      Right [S.Def (S.mkName "a") S.IntTy (S.Int 42)]
    it "def f(x:Int, y:Int):Int = x + y" $ P.parse (show (S.prettyDecl (S.Defun "f" S.IntTy [(S.mkName "x", S.IntTy), (S.mkName "y", S.IntTy)]
                                                                        (S.BinOp S.Add (S.Var (S.mkName "x")) (S.Var (S.mkName "y")))))) `shouldBe`
      Right [S.Defun "f" S.IntTy [(S.mkName "x", S.IntTy), (S.mkName "y", S.IntTy)]
                                                                  (S.BinOp S.Add (S.Var (S.mkName "x")) (S.Var (S.mkName "y")))]

main :: IO ()
main = hspec spec
