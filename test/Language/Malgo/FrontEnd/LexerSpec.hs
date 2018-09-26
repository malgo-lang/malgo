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
    t0 <- lex "<test>" ("let x = 42" :: Text)
    it "let" $ t0 `shouldBe` Right
      [ Loc (ss 1 1 1 5) LET
      , Loc (ss 1 5 1 7) (ID "x")
      , Loc (ss 1 7 1 9) EQUAL
      , Loc (ss 1 9 1 11) (INT 42)]
    t1 <- lex "<test>" ("let rec f x = x + 42" :: Text)
    it "let rec" $ t1 `shouldBe` Right
      [ Loc (ss 1 1 1 5) LET
      , Loc (ss 1 5 1 9) REC
      , Loc (ss 1 9 1 11) (ID "f")
      , Loc (ss 1 11 1 13) (ID "x")
      , Loc (ss 1 13 1 15) EQUAL
      , Loc (ss 1 15 1 17) (ID "x")
      , Loc (ss 1 17 1 19) PLUS
      , Loc (ss 1 19 1 21) (INT 42)]
    t2 <- lex "<test>" ("Tycon1 var1 Ty_Con2 vAr2" :: Text)
    it "Tycon" $ t2 `shouldBe` Right
      [ Loc (ss 1 1 1 8) (TYCON "Tycon1")
      , Loc (ss 1 8 1 13) (ID "var1")
      , Loc (ss 1 13 1 21) (TYCON "Ty_Con2")
      , Loc (ss 1 21 1 25) (ID "vAr2")]


ss :: Line -> Column -> Line -> Column -> SrcSpan
ss = SrcSpan "<test>"
