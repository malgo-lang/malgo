module Language.Malgo.IR where

import qualified Language.Malgo.Syntax as S

data IR = Var S.Name
        | Int Integer
        | Float Double
        | Bool Bool
        | Char Char
        | String String
        | Define S.Name Type IR
        | Add [IR]
        deriving (Eq, Show)

data Type = TSym S.Name
          | TList [Type]
          deriving (Eq, Show)

compile :: S.AST -> IR
compile (S.Symbol x) = Var x
compile (S.Int x)    = Int x
compile (S.Float x)  = Float x
compile (S.Bool x)   = Bool x
compile (S.Char x)   = Char x
compile (S.String x) = String x
compile (S.List xs)  = complist xs
compile x            = error ("not a valid form: " ++ show x)

complist :: [S.AST] -> IR
complist [S.Symbol "define", (S.:-:) (S.Symbol var) typ, val] = Define var (comptype typ) (compile val)
complist (S.Symbol "+" : args) = Add (map compile args)
complist x = error ("not a valid form: " ++ show x)

comptype :: S.AST -> Type
comptype (S.Symbol x) = TSym x
comptype (S.List xs)  = TList (map comptype xs)
comptype x            = error ("not a type: " ++ show x)
