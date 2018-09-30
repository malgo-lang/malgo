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
  lextest "Type enclosed in parentheses" "List (List Int)" [TYCON "List", LPAREN, TYCON "List", TYCON "Int", RPAREN]
  lextest "Function type" "Int -> Int -> Int" [TYCON "Int", ARROW, TYCON "Int", ARROW, TYCON "Int"]
  lextest "Type variable" "Tuple2 a b" [TYCON "Tuple2", ID "a", ID "b"]

  lextest "Variable" "x" [ID "x"]

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
