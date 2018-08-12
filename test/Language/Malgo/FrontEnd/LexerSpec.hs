{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.FrontEnd.LexerSpec (spec) where

import           Language.Malgo.FrontEnd.Lexer
import           Language.Malgo.FrontEnd.Loc
import           Language.Malgo.FrontEnd.Token
import           Universum
import           Test.Hspec

spec :: Spec
spec =
  describe "lexer" $ do
    t0 <- lex () "<test>" ("let x = 42" :: Text)
    it "let" $ t0 `shouldBe` Right
      [Loc x LET, Loc x (ID "x"), Loc x EQUAL, Loc x (INT 42)]
    t1 <- lex () "<test>" ("let rec f x = x + 42" :: Text)
    it "let rec" $ t1 `shouldBe` Right
      [Loc x LET, Loc x REC, Loc x (ID "f"), Loc x (ID "x"), Loc x EQUAL,
       Loc x (ID "x"), Loc x PLUS, Loc x (INT 42)]
    t2 <- lex () "<test>" ("let rec f x = (x+.1.0).println" :: Text)
    it "dot expr" $ t2 `shouldBe` Right
      [Loc x LET, Loc x REC, Loc x (ID "f"), Loc x (ID "x"), Loc x EQUAL,
       Loc x LPAREN, Loc x (ID "x"), Loc x PLUS_DOT, Loc x (FLOAT 1), Loc x RPAREN,
       Loc x DOT, Loc x (ID "println")]

x :: SrcSpan
x = SrcSpan "<test>" 0 0 0 0
