import           Test.Hspec

import qualified Language.Malgo.Parser as P
import qualified Language.Malgo.Syntax as S
import qualified Language.Malgo.Typing as T

import           Text.Parsec.Pos       (newPos)

parseTest = do
    it "def answer:Int = 42" $
      P.parse "def answer:Int = 42" `shouldBe`
        Right [S.Def (newPos "" 1 1) (S.mkName "answer") S.IntTy (S.Int (newPos "" 1 18) 42)]

    it "def pi:Float = 3.14" $ P.parse "def pi:Float = 3.14" `shouldBe`
      Right [S.Def (newPos "" 1 1) (S.mkName "pi") S.FloatTy (S.Float (newPos "" 1 16) 3.14)]

    it "def not(x:Bool):Bool = if x #f else #t" $
      P.parse "def not(x:Bool):Bool = if x #f else #t" `shouldBe`
      Right [S.Defun (newPos "" 1 1) (S.mkName "not") S.BoolTy
             [(S.mkName "x", S.BoolTy)]
             (S.If (newPos "" 1 27)
               (S.Var (newPos "" 1 27) (S.mkName "x"))
               (S.Bool (newPos "" 1 29) False)
               (S.Bool (newPos "" 1 37) True))]

    it "def prints():Unit = { print(\"hoge\"); print(\"foobar\") }" $
      P.parse "def prints():Unit = { print(\"hoge\"); print(\"foobar\") }" `shouldBe`
      Right [S.Defun (newPos "" 1 1) (S.mkName "prints")
             S.UnitTy []
             (S.Seq (newPos "" 1 36)
              (S.Call (newPos "" 1 23)
                (S.mkName "print")
                [S.String (newPos "" 1 29) "hoge"])
              (S.Call (newPos "" 1 38) (S.mkName "print")
               [S.String (newPos "" 1 44) "foobar"]))]

    it "def lezero(n:Int):Bool = if n <= 0 #t else #f" $
      P.parse "def lezero(n:Int):Bool = if n <= 0 #t else #f" `shouldBe`
        Right [S.Defun (newPos "" 1 1) (S.mkName "lezero") S.BoolTy
               [(S.mkName "n", S.IntTy)]
               (S.If (newPos "" 1 29)
                (S.BinOp (newPos "" 1 31) S.Le
                  (S.Var (newPos "" 1 29) (S.mkName "n"))
                  (S.Int (newPos "" 1 34) 0))
                (S.Bool (newPos "" 1 36) True)
                (S.Bool (newPos "" 1 44) False))]

    it "def answer:Int = let a:Int = 6 * 7; a" $
      P.parse "def answer:Int = let a:Int = 6 * 7; a" `shouldBe`
      Right [S.Def (newPos "" 1 1) (S.mkName "answer") S.IntTy
              (S.Seq (newPos "" 1 35)
               (S.Let (newPos "" 1 18) (S.mkName "a") S.IntTy
                (S.BinOp (newPos "" 1 32) S.Mul
                 (S.Int (newPos "" 1 30) 6)
                 (S.Int (newPos "" 1 34) 7)))
               (S.Var (newPos "" 1 37) (S.mkName "a")))]

dum = newPos "" 0 0

typingTest = do
  describe "Typing test(Expr)" $ do
    it "42" $ T.evalTypeofExpr (S.Int dum 42) `shouldBe` Right S.IntTy
    it "3.14" $ T.evalTypeofExpr (S.Float dum 3.14) `shouldBe` Right S.FloatTy
    it "#t" $ T.evalTypeofExpr (S.Bool dum True) `shouldBe` Right S.BoolTy
    it "'c'" $ T.evalTypeofExpr (S.Char dum 'c') `shouldBe` Right S.CharTy
    it "\"nyaaan\"" $ T.evalTypeofExpr (S.String dum "nyaaan") `shouldBe` Right S.StringTy
    it "unit" $ T.evalTypeofExpr (S.Unit dum) `shouldBe` Right S.UnitTy
    it "print(\"hello\")" $ T.evalTypeofExpr (S.Call dum (S.mkName "print") [S.String dum "hello"]) `shouldBe` Right S.UnitTy
    it "unit;1" $ T.evalTypeofExpr (S.Seq dum (S.Unit dum) (S.Int dum 1)) `shouldBe` Right S.IntTy
    it "if #t 1 else 2" $ T.evalTypeofExpr (S.If dum (S.Bool dum True) (S.Int dum 1) (S.Int dum 2)) `shouldBe` Right S.IntTy
    it "let a:Int = 1" $ T.evalTypeofExpr (S.Let dum (S.mkName "a") S.IntTy (S.Int dum 1)) `shouldBe` Right S.UnitTy
    it "let a:Int = 1; a" $ T.evalTypeofExpr (S.Seq dum
                                              (S.Let dum (S.mkName "a") S.IntTy (S.Int dum 1))
                                              (S.Var dum (S.mkName "a")))
      `shouldBe` Right S.IntTy
    it "1 + 2" $ T.evalTypeofExpr (S.BinOp dum S.Add (S.Int dum 1) (S.Int dum 2))
      `shouldBe` Right S.IntTy
    it "1 - 2" $ T.evalTypeofExpr (S.BinOp dum S.Sub (S.Int dum 1) (S.Int dum 2))
      `shouldBe` Right S.IntTy
    it "1 * 2" $ T.evalTypeofExpr (S.BinOp dum S.Mul (S.Int dum 1) (S.Int dum 2))
      `shouldBe` Right S.IntTy
    it "1 / 2" $ T.evalTypeofExpr (S.BinOp dum S.Div (S.Int dum 1) (S.Int dum 2))
      `shouldBe` Right S.IntTy

    it "1.0 + 2.0" $ T.evalTypeofExpr (S.BinOp dum S.Add (S.Float dum 1) (S.Float dum 2))
      `shouldBe` Right S.FloatTy
    it "1.0 - 2.0" $ T.evalTypeofExpr (S.BinOp dum S.Sub (S.Float dum 1) (S.Float dum 2))
      `shouldBe` Right S.FloatTy
    it "1.0 * 2.0" $ T.evalTypeofExpr (S.BinOp dum S.Mul (S.Float dum 1) (S.Float dum 2))
      `shouldBe` Right S.FloatTy
    it "1.0 / 2.0" $ T.evalTypeofExpr (S.BinOp dum S.Div (S.Float dum 1) (S.Float dum 2))
      `shouldBe` Right S.FloatTy

  describe "Typing test(Decl)" $ do
    it "def a:Int = 42" $
      T.evalTypeofDecl (S.Def dum (S.mkName "a") S.IntTy (S.Int dum 42))
      `shouldBe` Right S.IntTy

    it "def f(x:Int, y:Int):Int = x + y" $
      T.evalTypeofDecl (S.Defun dum (S.mkName "f") S.IntTy
                        [(S.mkName "x", S.IntTy), (S.mkName "y", S.IntTy)]
                        (S.BinOp dum S.Add (S.Var dum (S.mkName "x")) (S.Var dum (S.mkName "y"))))
        `shouldBe` Right (S.FunTy S.IntTy [S.IntTy, S.IntTy])

    it "def g(x:Int):Int = if #t 0 else g(x-1)" $
      T.evalTypeofDecl (S.Defun dum (S.mkName "g") S.IntTy [(S.mkName "x", S.IntTy)]
                        (S.If dum
                         (S.Bool dum True)
                         (S.Int dum 0)
                         (S.Call dum (S.mkName "g") [S.BinOp dum S.Sub (S.Var dum (S.mkName "x")) (S.Int dum 1)])))
        `shouldBe` Right (S.FunTy S.IntTy [S.IntTy])

    it "def lezero(n:Int):Bool = if n <= 0 #t else #f" $
      T.evalTypeofDecl (S.Defun dum (S.mkName "lezero") S.BoolTy [(S.mkName "n", S.IntTy)]
                        (S.If dum
                         (S.BinOp dum S.Le (S.Var dum (S.mkName "n")) (S.Int dum 0))
                         (S.Bool dum True)
                         (S.Bool dum False)))
      `shouldBe` Right (S.FunTy S.BoolTy [S.IntTy])


spec = do
  describe "Parse test" parseTest
  typingTest
  -- describe "Pretty Printer test" $ do
  --   it "def a:Int = 42" $ P.parse (show (S.prettyDecl (S.Def (S.mkName "a") S.IntTy (S.Int 42)))) `shouldBe`
  --     Right [S.Def (S.mkName "a") S.IntTy (S.Int 42)]
  --   it "def f(x:Int, y:Int):Int = x + y" $ P.parse (show (S.prettyDecl (S.Defun "f" S.IntTy [(S.mkName "x", S.IntTy), (S.mkName "y", S.IntTy)]
  --                                                                       (S.BinOp S.Add (S.Var (S.mkName "x")) (S.Var (S.mkName "y")))))) `shouldBe`
  --     Right [S.Defun "f" S.IntTy [(S.mkName "x", S.IntTy), (S.mkName "y", S.IntTy)]
  --                                                                 (S.BinOp S.Add (S.Var (S.mkName "x")) (S.Var (S.mkName "y")))]

main :: IO ()
main = hspec spec
