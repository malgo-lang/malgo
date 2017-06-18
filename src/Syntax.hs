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
(let ((a:int (read-int)) (b:int (read-int)))
  (print-int (gcd a b)))
-}

type Name = String
type Type = String

data Func = Func { name       :: Name
                 , returnType :: Type
                 , params     :: [(Name, Type)]
                 , body       :: [Expr]
                 }
  -- deriving Show

instance Show Func where
  show Func { name = name
            , returnType = returnType
            , params = params
            , body = body } = "(" ++ name ++ ":" ++ returnType ++ " " ++ showParams params ++ ") " ++ showBody body
    where showParams []            = ""
          showParams [(name, typ)] = name ++ ":" ++ typ
          showParams ((name, typ):ns) = name ++ ":" ++ typ ++ " " ++ showParams ns
          showBody []     = ""
          showBody [e]    = show e
          showBody (e:es) = show e ++ " " ++ showBody es

data Expr = Nil
          | Int Int
          | Bool Bool
          | Float Double
          -- | Cell Expr Expr
          | Defn Func
          | Call Name [Expr]
          | Var Name
          | If Expr Expr Expr
          | Let [(Name, Type, Expr)] [Expr]

instance Show Expr where
  show Nil           = ""
  show (Int i)       = show i
  show (Bool b)      = show b
  show (Float f)     = show f
  show (Defn f)      = "(defn " ++ show f ++ ")"
  show (Call n [])   = "(" ++ n ++ ")"
  show (Call n args) = "(" ++ n ++ " " ++ showArgs args ++ ")"
    where showArgs []     = ""
          showArgs [e]    = show e
          showArgs (e:es) = show e ++ " " ++ showArgs es
  show (Var n)       = n
  show (If p t e)    = "(if " ++ show p ++ " " ++ show t ++ " " ++ show e ++ ")"
  show (Let defs es) = "(let " ++ "(" ++ showDefs defs ++ ") " ++ showBody es ++ ")"
    where showDefs [] = ""
          showDefs [(n, ty, e)] = "(" ++  n ++ ":" ++ ty ++ " " ++ show e ++ ")"
          showDefs ((n, ty, e):ds) = "(" ++ n ++ ":" ++ ty ++ " " ++ show e ++ ") " ++ showDefs ds
          showBody []     = ""
          showBody [e]    = show e
          showBody (e:es) = show e ++ " " ++ showBody es

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
printgcd = Let [ ("a", "int", Call "read-int" [])
               , ("b", "int", Call "read-int" [])
               ] [Call "gcd" [Var "a", Var "b"]]

sample :: [Expr]
sample = [defnAdd, Call "add" [Int 1, Int 2], defnGcd, printgcd]
