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
    it "Parametized type" $
      parseType (toks [TYCON "List", TYCON "Int"])
      `shouldBe` STyApp (SimpleC "List") [atype "Int"]
    it "Type enclosed in parentheses" $
      parseType (toks [TYCON "List", LPAREN, TYCON "List", TYCON "Int", RPAREN])
      `shouldBe` STyApp (SimpleC "List") [STyApp (SimpleC "List") [atype "Int"]]
    it "Function type" $
      parseType (toks [TYCON "Int", ARROW, TYCON "Int", ARROW, TYCON "Int"])
      `shouldBe` STyApp (SimpleC "->") [atype "Int", STyApp (SimpleC "->") [atype "Int", atype "Int"]]
    it "Type variable" $
      parseType (toks [TYCON "Tuple2", ID "a", ID "b"])
      `shouldBe` STyApp (SimpleC "Tuple2") [STyVar "a", STyVar "b"]

  describe "parseExpr" $ do
    it "Variable" $
      parseExpr [tok $ ID "x"]
      `shouldBe` Var ss "x"
    it "Int literal" $
      parseExpr [tok $ INT 42]
      `shouldBe` Literal ss (Int 42)
    it "Float literal" $
      parseExpr [tok $ FLOAT 3.14]
      `shouldBe` Literal ss (Float 3.14)
    it "True literal" $
      parseExpr [tok TRUE]
      `shouldBe` Literal ss (Bool True)
    it "False literal" $
      parseExpr [tok FALSE]
      `shouldBe` Literal ss (Bool False)
    it "Char literal" $
      parseExpr [tok $ CHAR 'c']
      `shouldBe` Literal ss (Char 'c')

  describe "parseDecl" $ do
    it "id function" $
      parseDecl (toks [ID "f", ID "x", EQUAL, ID "x"])
      `shouldBe` ScDef ss "f" ["x"] (Var ss "x")

    it "type signature 1" $
      parseDecl (toks [ID "x", COLON, TYCON "Int"])
      `shouldBe` ScAnn ss "x" (atype "Int")

    it "type signature 2" $
      parseDecl (toks [ID "f", COLON, TYCON "Int", ARROW, TYCON "Int"])
      `shouldBe` ScAnn ss "f" (STyApp (SimpleC "->") [atype "Int", atype "Int"])


tok :: Tag -> Loc Tag
tok = Loc ss

ss :: SrcSpan
ss = SrcSpan "<test>" 0 0 0 0

atype :: Text -> SType
atype x = STyApp (SimpleC x) []

toks :: [Tag] -> [Loc Tag]
toks xs = map tok xs
