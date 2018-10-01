{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.FrontEnd.LexerSpec (spec) where

import           Language.Malgo.FrontEnd.Lexer
import           Language.Malgo.FrontEnd.Loc
import           Language.Malgo.FrontEnd.Token
import           Universum
import           Test.Hspec

spec :: Spec
spec = describe "lexer" $ do
  lextest "Atomic type" "Int" [TYCON "Int"]
  lextest "Parametized type" "List Int" [TYCON "List", TYCON "Int"]
  lextest "Type enclosed in parentheses"
    "List (List Int)"
    [TYCON "List", LPAREN, TYCON "List", TYCON "Int", RPAREN]
  lextest "Function type"
    "Int -> Int -> Int"
    [TYCON "Int", ARROW, TYCON "Int", ARROW, TYCON "Int"]
  lextest "Type variable" "Tuple2 a b" [TYCON "Tuple2", ID "a", ID "b"]
  lextest "Record type"
    "{x: Int, y : List Int }"
    [LBRACE, ID "x", COLON, TYCON "Int", COMMA, ID "y", COLON, TYCON "List", TYCON "Int", RBRACE]
  lextest "Variant type"
    "< x:Int, y:List Int>"
    [LT_OP, ID "x", COLON, TYCON "Int", COMMA, ID "y", COLON, TYCON "List", TYCON "Int", GT_OP]

  lextest "Variable" "x" [ID "x"]
  lextest "Int literal" "42" [INT 42]
  lextest "Float literal" "3.14" [FLOAT 3.14]
  lextest "True literal" "true" [TRUE]
  lextest "False literal" "false" [FALSE]
  lextest "Char literal" "'c'" [CHAR 'c']
  lextest "Record"
    "{x = 42, y = a}"
    [LBRACE, ID "x", EQUAL, INT 42, COMMA, ID "y", EQUAL, ID "a", RBRACE]
  lextest "Variant"
    "<x = 42, y : List Int>"
    [LT_OP, ID "x", EQUAL, INT 42, COMMA, ID "y", COLON, TYCON "List", TYCON "Int", GT_OP]
  lextest "Anonymous function"
    "fn x (y : Int) -> x"
    [FN, ID "x", LPAREN, ID "y", COLON, TYCON "Int", RPAREN, ARROW, ID "x"]
  lextest "let expression with type annotation"
    "let x : Int = 42 in x"
    [LET, ID "x", COLON, TYCON "Int", EQUAL, INT 42, IN, ID "x"]
  lextest "let expression without type annotation"
    "let x = 42 in x"
    [LET, ID "x", EQUAL, INT 42, IN, ID "x"]
  lextest "let rec expression with type annotation"
    "let rec f x : Int -> Int = x in f"
    [LET, REC, ID "f", ID "x", COLON, TYCON "Int", ARROW, TYCON "Int", EQUAL, ID "x", IN, ID "f"]
  lextest "let rec expression without type annotation"
    "let rec f x = x in f"
    [LET, REC, ID "f", ID "x", EQUAL, ID "x", IN, ID "f"]
  lextest "let rec expression that declare mutural recursive variables"
    "let rec x = x and y = x in y"
    [LET, REC, ID "x", EQUAL, ID "x", AND, ID "y", EQUAL, ID "x", IN, ID "y"]
  lextest "Case expression"
    "case x { | x => x }"
    [CASE, ID "x", LBRACE, OR_OP, ID "x", DARROW, ID "x", RBRACE]

  lextest "id function" "f x = x" [ID "f", ID "x", EQUAL, ID "x"]
  lextest "type signature 1" "x : Int" [ID "x", COLON, TYCON "Int"]
  lextest "type signature 2" "f : Int -> Int" [ID "f", COLON, TYCON "Int", ARROW, TYCON "Int"]

ss :: SrcSpan
ss = SrcSpan "<test>" 0 0 0 0

tok :: a -> Loc a
tok = Loc ss

toks :: [a1] -> Either a2 [Loc a1]
toks = Right . map tok

lextest :: String -> Text -> [Tag] -> SpecWith ()
lextest desc str ts = do
  t <- lex "<test>" (str :: Text)
  it desc $ t `shouldBe` toks ts
