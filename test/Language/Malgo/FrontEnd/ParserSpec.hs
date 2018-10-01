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
    it "Record type" $
      parseType (toks [LBRACE, ID "x", COLON, TYCON "Int", COMMA, ID "y", COLON, TYCON "List", TYCON "Int", RBRACE])
      `shouldBe` STyApp (SRecordC [("x", atype "Int"), ("y", STyApp (SimpleC "List") [atype "Int"])]) []
    it "Variant type" $
      parseType (toks [LT_OP, ID "x", COLON, TYCON "Int", COMMA, ID "y", COLON, TYCON "List", TYCON "Int", GT_OP])
      `shouldBe` STyApp (SVariantC [("x", atype "Int"), ("y", STyApp (SimpleC "List") [atype "Int"])]) []

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
    it "Record" $
      parseExpr (toks [LBRACE, ID "x", EQUAL, INT 42, COMMA, ID "y", EQUAL, ID "a", RBRACE])
      `shouldBe` Record ss [("x", Literal ss (Int 42)), ("y", Var ss "a")]
    it "Variant" $
      parseExpr (toks [LT_OP, ID "x", EQUAL, INT 42, COMMA, ID "y", COLON, TYCON "List", TYCON "Int", GT_OP])
      `shouldBe` Variant ss "x" (Literal ss (Int 42)) [("y", STyApp (SimpleC "List") [atype "Int"])]
    it "Anonymous function" $
      parseExpr (toks [FN, ID "x", LPAREN, ID "y", COLON, TYCON "Int", RPAREN, ARROW, ID "x"])
      `shouldBe` Fn ss [("x", Nothing), ("y", Just (atype "Int"))] (Var ss "x")
    it "let expression with type annotation" $
      parseExpr (toks [LET, ID "x", COLON, TYCON "Int", EQUAL, INT 42, IN, ID "x"])
      `shouldBe` Let ss (NonRec ss "x" (Just (atype "Int")) (Literal ss (Int 42))) (Var ss "x")
    it "let expression without type annotation" $
      parseExpr (toks [LET, ID "x", EQUAL, INT 42, IN, ID "x"])
      `shouldBe` Let ss (NonRec ss "x" Nothing (Literal ss (Int 42))) (Var ss "x")
    it "let rec expression with type annotation" $
      parseExpr (toks [LET, REC, ID "f", ID "x", COLON, TYCON "Int", ARROW, TYCON "Int", EQUAL, ID "x", IN, ID "f"])
      `shouldBe` Let ss (Rec [(ss, "f", Just (STyApp (SimpleC "->") [atype "Int", atype "Int"]), ["x"], Var ss "x")]) (Var ss "f")
    it "let rec expression without type annotation" $
      parseExpr (toks [LET, REC, ID "f", ID "x", EQUAL, ID "x", IN, ID "f"])
      `shouldBe` Let ss (Rec [(ss, "f", Nothing, ["x"], Var ss "x")]) (Var ss "f")
    it "let rec expression that declare mutural recursive variables" $
      parseExpr (toks [LET, REC, ID "x", EQUAL, ID "x", AND, ID "y", EQUAL, ID "x", IN, ID "y"])
      `shouldBe` Let ss (Rec [ (ss, "x", Nothing, [], Var ss "x")
                             , (ss, "y", Nothing, [], Var ss "x")]) (Var ss "y")
    it "Case expression" $
      parseExpr (toks [CASE, ID "x", LBRACE, OR_OP, ID "x", DARROW, ID "x", RBRACE])
      `shouldBe` Case ss (Var ss "x") [VarPat ss "x" (Var ss "x")]

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
