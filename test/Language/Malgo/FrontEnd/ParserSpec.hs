{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.FrontEnd.ParserSpec where

import           Language.Malgo.FrontEnd.Loc
import           Language.Malgo.FrontEnd.Parser
import           Language.Malgo.FrontEnd.Token
import           Language.Malgo.IR.AST
import           Test.Hspec
import           Universum

spec :: Spec
spec = do
  describe "parseType" $ do
    it "Atomic type" $
      parseType [tok (TYCON "Int")]
      `shouldBe` atype "Int"

  describe "parseExpr" $ do
    it "Variable" $
      parseExpr [tok (ID "x")]
      `shouldBe` Var ss "x"

  describe "parseDecl" $ do
    it "id function" $
      parseDecl [tok (ID "f"), tok (ID "x"), tok EQUAL, tok (ID "x")]
      `shouldBe` ScDef ss "f" ["x"] (Var ss "x")

    it "type signature 1" $
      parseDecl [tok (ID "x"), tok COLON, tok (TYCON "Int")]
      `shouldBe` ScAnn ss "x" (atype "Int")

    it "type signature 2" $
      parseDecl [tok (ID "f"), tok COLON, tok (TYCON "Int"), tok ARROW, tok (TYCON "Int")]
      `shouldBe` ScAnn ss "f" (STyApp (SimpleC "->") [atype "Int", atype "Int"])


tok :: a -> Loc a
tok = Loc ss

ss :: SrcSpan
ss = SrcSpan "<test>" 0 0 0 0

atype :: Text -> SType
atype x = STyApp (SimpleC x) []
