module Language.Malgo.Syntax where

type Name = String

data AST = Symbol Name
         | Int Integer
         | Float Double
         | Bool Bool
         | Char Char
         | String String
         | List [AST]
         | AST :-: AST
         deriving (Eq, Show)

{--
(define (f:int) 5)
=> List [Symbol "define", List [Symbol "f" :-: Symbol "int"], Int 5]
(+ 3 4 5)
=> List [Symbol "+", Int 3, Int 4, Int 5]
(if (= x 8) (print "hoge") (print "fuga"))
=> List [Symbol "if", List [Symbol "=", Symbol "x", Int 8]
                    , List [Symbol "print", String "hoge"]
                    , List [Symbol "print", String "fuga"]]
-}
