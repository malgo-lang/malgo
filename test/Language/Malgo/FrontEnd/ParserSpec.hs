{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
module Language.Malgo.FrontEnd.ParserSpec where

import           Language.Malgo.FrontEnd.Loc
import           Language.Malgo.FrontEnd.Parser
import           Language.Malgo.FrontEnd.Token
import           Language.Malgo.IR.AST
import           Language.Malgo.Type
import           Test.Hspec
import           Universum                      hiding (Type)

spec :: Spec
spec = describe "Parser" $ do
  parseTest "annotation"
    [ID "f", COLON, FORALL, ID "a", DOT, ID "a", ARROW, ID "a", SEMICOLON]
    [ScAnn ss "f" (Forall ["a"] (TyApp (PrimC ArrowC) [TyVar "a", TyVar "a"]))]
  parseTest "function definition"
    [ID "f", ID "x", EQUAL, ID "x", SEMICOLON]
    [ScDef ss "f" ["x"] (Var ss "x")]
  parseTest "type alias definition"
    [TYPE, LID "T", EQUAL, LID "Array", LID "Int", SEMICOLON]
    [TypeDef ss "T" [] (TyApp (SimpleC "Array") [TyApp (SimpleC "Int") []])]

  parseExprTest "int literal" [INT 42] (Literal ss (Int 42))
  parseExprTest "float literal" [FLOAT 3.14] (Literal ss (Float 3.14))
  parseExprTest "bool literal 1" [TRUE] (Literal ss (Bool True))
  parseExprTest "bool literal 2" [FALSE] (Literal ss (Bool False))
  parseExprTest "char literal" [CHAR 'c'] (Literal ss (Char 'c'))
  parseExprTest "string literal" [STRING "str"] (Literal ss (String "str"))

  parseExprTest "4 * 10 + 2"
    [INT 4, ASTERISK, INT 10, PLUS, INT 2]
    (BinOp ss Add
     (BinOp ss Mul (Literal ss (Int 4)) (Literal ss (Int 10)))
     (Literal ss (Int 2)))
  parseExprTest "negate"
    [MINUS, INT 43, MINUS, MINUS, INT 1]
    (BinOp ss Sub
     (Literal ss (Int $ -43))
     (Literal ss (Int $ -1)))
  parseExprTest "x - 1"
    [ID "x", MINUS, INT 1]
    (BinOp ss Sub
     (Var ss "x")
     (Literal ss (Int 1)))
  parseExprTest "x - -1"
    [ID "x", MINUS, MINUS, INT 1]
    (BinOp ss Sub
     (Var ss "x")
     (Literal ss (Int $ -1)))
  parseExprTest "3.0 +. 0.14"
    [FLOAT 3.0, PLUS_DOT, FLOAT 0.14]
    (BinOp ss FAdd
     (Literal ss (Float 3.0))
     (Literal ss (Float 0.14)))

  parseExprTest "unit"
    [LPAREN, RPAREN] (Tuple ss [])
  parseExprTest "tuple"
    [LPAREN, INT 1, COMMA, INT 2, RPAREN]
    (Tuple ss [Literal ss $ Int 1, Literal ss $ Int 2])

  parseExprTest "simple apply"
    [ID "f", ID "x"]
    (Apply ss (Var ss "f") (Var ss "x"))
  parseExprTest "multiple arguments"
    [ID "f", ID "x", ID "y"]
    (Apply ss
     (Apply ss (Var ss "f") (Var ss "x"))
     (Var ss "y"))

  parseExprTest "function call and arithmetic expression"
    [ID "f", INT 1, PLUS, INT 2]
    (BinOp ss Add
     (Apply ss (Var ss "f") (Literal ss (Int 1)))
     (Literal ss (Int 2)))

  parseExprTest "if expression"
    [IF, ID "c", THEN, IF, ID "d", THEN, INT 1, ELSE, INT 2, ELSE, INT 3]
    (If ss (Var ss "c")
     (If ss (Var ss "d")
      (Literal ss (Int 1))
      (Literal ss (Int 2)))
     (Literal ss (Int 3)))

  parseExprTest "let expression 1"
    [LET, ID "x", EQUAL, INT 1, IN, ID "x"]
    (Let ss (NonRec ss "x" Nothing (Literal ss (Int 1))) (Var ss "x"))
  parseExprTest "let expression 2"
    [LET, ID "x", EQUAL, LET, ID "y", EQUAL, INT 1, IN, ID "y", IN, ID "x"]
    (Let ss (NonRec ss "x" Nothing
             (Let ss (NonRec ss "y" Nothing (Literal ss (Int 1))) (Var ss "y")))
      (Var ss "x"))
  parseExprTest "let expression 3"
    [LET, ID "x", COLON, LID "Int", EQUAL, INT 1, IN, ID "x"]
    (Let ss (NonRec ss "x" (Just $ TyApp (SimpleC "Int") []) (Literal ss (Int 1))) (Var ss "x"))
  parseExprTest "let expression 4"
    [LET, REC, ID "f", ID "x", EQUAL, ID "x", IN, ID "f"]
    (Let ss (Rec ss "f" [("x", Nothing)] Nothing (Var ss "x")) (Var ss "f"))
  parseExprTest "let expression 5"
    [LET, REC, ID "f", LPAREN, ID "x", COLON, LID "Int", RPAREN, COLON, LID "Int", EQUAL, ID "x", IN, ID "f"]
    (Let ss (Rec ss "f" [("x", Just $ TyApp (SimpleC "Int") [])] (Just $ TyApp (SimpleC "Int") []) (Var ss "x")) (Var ss "f"))

ss :: SrcSpan
ss = SrcSpan "<test>" 0 0 0 0

parseTest :: String -> [Tag] -> [Decl Text] -> SpecWith ()
parseTest desc ts ast =
  it desc $ parse (map (Loc ss) ts) `shouldBe` ast

parseExprTest :: String -> [Tag] -> Expr Text -> SpecWith ()
parseExprTest desc ts ast =
  it desc $ parse (map (Loc ss) $ [ID "f", EQUAL] <> ts <> [SEMICOLON])
  `shouldBe` [ScDef ss "f" [] ast]
