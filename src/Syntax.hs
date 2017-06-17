module Syntax (Name, Type, Expr(..)) where

{-
(defn add:int (x:int y:int)
  (+ x y))
-}

type Name = String
type Type = String

data Expr = Nil
          | Defn { name       :: Name
                 , returnType :: Type
                 , params     :: [(Name, Type)]
                 , body       :: [Expr]}
          | Call Name [Expr]
          | Var Name
          deriving (Show, Eq)

defnAdd :: Expr
defnAdd = Defn { name = "add"
               , returnType = "int"
               , params = [("x", "int"), ("y", "int")]
               , body = [Call "+" [Var "x", Var "y"]]
               }
