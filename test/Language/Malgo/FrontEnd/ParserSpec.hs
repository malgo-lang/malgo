{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.FrontEnd.ParserSpec where

import           Language.Malgo.FrontEnd.Loc
import           Language.Malgo.FrontEnd.Parser
import           Language.Malgo.FrontEnd.Token
import           Language.Malgo.IR.AST
import           Test.Hspec
import           Universum

spec :: Spec
spec = describe "parser" $ do
  let a0 = parse [tok (ID "f"), tok (ID "x"), tok EQUAL, tok (ID "x")]
  it "id function" $ a0 `shouldBe` [ScDef ss "f" ["x"] (Var ss "x")]

  let a1 = parse [tok (ID "x"), tok COLON, tok (TYCON "Int")]
  it "type signature 1" $ a1 `shouldBe` [ScAnn ss "x" (STyApp (SimpleC "Int") [])]

  let a2 = parse [tok (ID "f"), tok COLON, tok (TYCON "Int"), tok ARROW, tok (TYCON "Int")]
  it "type signature 2" $ a2 `shouldBe` [ScAnn ss "f" (STyApp (SimpleC "->") [STyApp (SimpleC "Int") [], STyApp (SimpleC "Int") []])]


tok :: a -> Loc a
tok = Loc ss

ss :: SrcSpan
ss = SrcSpan "<test>" 0 0 0 0
