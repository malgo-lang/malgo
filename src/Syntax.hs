module Syntax (Name, Type, Func(..), Expr(..)) where

{-
TODO: より良いプリティプリンタ
(define (add:int x:int y:int)
  (+ x y))
(define (gcd:int m:int n:int)
  (if (= m 0)
      n
      (if (<= m n)
          (gcd m (- n m))
          (gcd n (- m n)))))
(define answer:int 42)
(let ((a:int (read-int)))
  (print-int (gcd a ans)))
-}

type Name = String
type Type = String

data Func = Func { name       :: Name
                 , returnType :: Type
                 , params     :: [(Name, Type)]
                 , body       :: [Expr]
                 }
  deriving Show

data Expr = Nil
          | Int Int
          | Bool Bool
          -- | Float Double
          | Defn Func
          | Def Name Type Expr
          | Call Name [Expr]
          | Var Name
          | If Expr Expr Expr
          | Let (Name, Type, Expr) [Expr]
  deriving Show

defnAdd :: Expr
defnAdd = Defn $ Func { name = "add"
                      , returnType = "int"
                      , params = [("x", "int"), ("y", "int")]
                      , body = [Call "+" [Var "x", Var "y"]]
                      }

defnGcd :: Expr
defnGcd = Defn $ Func
  { name = "gcd"
  , returnType = "int"
  , params = [("m", "int"), ("n", "int")]
  , body = [If (Call "=" [Var "m", Int 0])
             (Var "n")
             (If (Call "<=" [Var "m", Var "n"])
               (Call "gcd" [ Var "m"
                           , Call "-" [Var "n", Var "m"]])
               (Call "gcd" [ Var "n"
                           , Call "-" [Var "m", Var "n"]]))]
  }

printgcd :: Expr
printgcd =
  Let ("a", "int", Call "read-int" []) [
  Let ("b", "int", Call "read-int" []) [
      Call "gcd" [Var "a", Var "b"]
      ]
  ]

sample :: [Expr]
sample = [defnAdd, Call "add" [Int 1, Int 2], defnGcd, printgcd]
