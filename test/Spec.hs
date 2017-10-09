import           Test.Hspec

import           Language.Malgo.HIR
import qualified Language.Malgo.HIR    as H
import qualified Language.Malgo.Parser as P
import           Language.Malgo.Syntax
import qualified Language.Malgo.Syntax as S
import qualified Language.Malgo.Typing as T

import           Text.Parsec.Pos       (newPos)

parseTest = do
    it "def answer:Int = 42" $
      P.parse "def answer:Int = 42" `shouldBe`
        Right [S.Def (newPos "" 1 1) (S.mkName "answer") S.IntTy (S.Int (newPos "" 1 18) 42)]

    it "def pi:Float = 3.14" $ P.parse "def pi:Float = 3.14" `shouldBe`
      Right [S.Def (newPos "" 1 1) (S.mkName "pi") S.FloatTy (S.Float (newPos "" 1 16) 3.14)]

    it "extern print(str:String):Unit" $ P.parse "extern print(str:String):Unit" `shouldBe`
      Right [S.ExDefun (newPos "" 1 1) (S.mkName "print") S.UnitTy [(S.mkName "str", S.StringTy)]]

    it "extern pi:Float" $ P.parse "extern pi:Float" `shouldBe`
      Right [S.ExDef (newPos "" 1 1) (S.mkName "pi") S.FloatTy]

    it "def not(x:Bool):Bool = if x #f else #t" $
      P.parse "def not(x:Bool):Bool = if x #f else #t" `shouldBe`
      Right [S.Defun (newPos "" 1 1) (S.mkName "not") S.BoolTy
             [(S.mkName "x", S.BoolTy)]
             (S.If (newPos "" 1 24)
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
               (S.If (newPos "" 1 26)
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
    it "42" $ T.evalTypedExpr (S.Int dum 42) `shouldBe` Right (H.INT 42, S.IntTy)
    it "3.14" $ T.evalTypedExpr (S.Float dum 3.14) `shouldBe` Right (H.FLOAT 3.14, S.FloatTy)
    it "#t" $ T.evalTypedExpr (S.Bool dum True) `shouldBe` Right (BOOL True, BoolTy)
    it "'c'" $ T.evalTypedExpr (S.Char dum 'c') `shouldBe` Right (CHAR 'c', CharTy)
    it "\"nyaaan\"" $ T.evalTypedExpr (S.String dum "nyaaan") `shouldBe` Right (STRING "nyaaan", StringTy)
    it "unit" $ T.evalTypedExpr (S.Unit dum) `shouldBe` Right (UNIT, UnitTy)
    it "print(\"hello\")" $ T.evalTypedExpr (S.Call dum (S.mkName "print") [S.String dum "hello"]) `shouldBe`
      Right (CALL (name2Id "print") [(STRING "hello", StringTy)], UnitTy)

    it "unit;1" $ T.evalTypedExpr (S.Seq dum (S.Unit dum) (S.Int dum 1)) `shouldBe`
      Right (SEQ (UNIT,UnitTy) (INT 1,IntTy),IntTy)

    it "if #t 1 else 2" $ T.evalTypedExpr (S.If dum
                                           (S.Bool dum True)
                                           (S.Int dum 1)
                                           (S.Int dum 2)) `shouldBe`
      Right (IF (BOOL True,BoolTy) (INT 1,IntTy) (INT 2,IntTy),IntTy)

    it "let a:Int = 1" $ T.evalTypedExpr (S.Let dum
                                          (S.mkName "a") S.IntTy
                                          (S.Int dum 1)) `shouldBe`
      Right (LET (Sym {id2name = "a"}) IntTy (INT 1,IntTy),UnitTy)

    it "let a:Int = 1; a" $ T.evalTypedExpr (S.Seq dum
                                              (S.Let dum (S.mkName "a") S.IntTy (S.Int dum 1))
                                              (S.Var dum (S.mkName "a")))
      `shouldBe` Right (SEQ
                        (LET (Sym {id2name = "a"}) IntTy (INT 1,IntTy),UnitTy)
                        (VAR (Sym {id2name = "a"}),IntTy),IntTy)

  describe "Typing test(Decl)" $ do
    it "def a:Int = 42" $
      T.evalTypedDecl (S.Def dum (S.mkName "a") S.IntTy (S.Int dum 42))
      `shouldBe` Right (DEF (Sym {id2name = "a"}) IntTy (INT 42,IntTy))

    it "def f(x:Int, y:Int):Int = x + y" $
      T.evalTypedDecl (S.Defun dum (S.mkName "f") S.IntTy
                        [(S.mkName "x", S.IntTy), (S.mkName "y", S.IntTy)]
                        (S.BinOp dum S.Add (S.Var dum (S.mkName "x")) (S.Var dum (S.mkName "y"))))
        `shouldBe` Right (DEFUN (Sym {id2name = "f"}) IntTy
                          [(Sym {id2name = "x"},IntTy),(Sym {id2name = "y"},IntTy)]
                          (BINOP Add
                           (VAR (Sym {id2name = "x"}),IntTy)
                           (VAR (Sym {id2name = "y"}),IntTy),IntTy))

    it "def g(x:Int):Int = if #t 0 else g(x-1)" $
      T.evalTypedDecl (S.Defun dum (S.mkName "g") S.IntTy [(S.mkName "x", S.IntTy)]
                        (S.If dum
                         (S.Bool dum True)
                         (S.Int dum 0)
                         (S.Call dum (S.mkName "g") [S.BinOp dum S.Sub (S.Var dum (S.mkName "x")) (S.Int dum 1)])))
        `shouldBe` Right (DEFUN (Sym {id2name = "g"}) IntTy
                          [(Sym {id2name = "x"},IntTy)]
                          (IF (BOOL True,BoolTy)
                           (INT 0,IntTy)
                           (CALL (Sym {id2name = "g"})
                            [(BINOP Sub
                              (VAR (Sym {id2name = "x"}),IntTy)
                              (INT 1,IntTy),IntTy)],IntTy),IntTy))

    it "def lezero(n:Int):Bool = if n <= 0 #t else #f" $
      T.evalTypedDecl (S.Defun dum (S.mkName "lezero") S.BoolTy [(S.mkName "n", S.IntTy)]
                        (S.If dum
                         (S.BinOp dum S.Le (S.Var dum (S.mkName "n")) (S.Int dum 0))
                         (S.Bool dum True)
                         (S.Bool dum False)))
      `shouldBe` Right (DEFUN (Sym {id2name = "lezero"}) BoolTy
                        [(Sym {id2name = "n"},IntTy)]
                        (IF (BINOP Le
                             (VAR (Sym {id2name = "n"}),IntTy)
                             (INT 0,IntTy),BoolTy)
                          (BOOL True,BoolTy)
                          (BOOL False,BoolTy),BoolTy))


    it "def a:Int = {{let b:Int = 42; print_int(42)}; b}" $
      T.evalTypedDecl (S.Def dum (S.mkName "a") S.IntTy
                        (S.Seq dum (S.Seq dum (S.Let dum (S.mkName "b") S.IntTy (S.Int dum 42))
                                    (S.Call dum (S.mkName "print_int") [S.Int dum 42]))
                         (S.Var dum "b")))
      `shouldBe` Left "error: \"b\" is not defined.\n"

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
