{-# LANGUAGE NoMonomorphismRestriction #-}
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
    [ScAnn ss "f" (Forall ["a"] (TyApp ArrowC [TyVar "a", TyVar "a"]))]
  parseTest "function definition"
    [ID "f", ID "x", EQUAL, ID "x", SEMICOLON]
    [ScDef ss "f" ["x"] (Var ss "x")]
  parseTest "type alias definition"
    [TYPE, LID "T", EQUAL, LID "Array", LID "Int", SEMICOLON]
    [TypeDef ss "T" [] (TyApp (SimpleC "Array") [TyApp (SimpleC "Int") []])]

  parseExprTest "arithmetic expression"
    [INT 4, ASTERISK, INT 10, PLUS, INT 2]
    (BinOp ss Add
     (BinOp ss Mul (Literal ss (Int 4)) (Literal ss (Int 10)))
     (Literal ss (Int 2)))

ss :: SrcSpan
ss = SrcSpan "<test>" 0 0 0 0

parseTest :: String -> [Tag] -> [Decl Text] -> SpecWith ()
parseTest desc ts ast =
  it desc $ parse (map (Loc ss) ts) `shouldBe` ast

parseExprTest :: String -> [Tag] -> Expr Text -> SpecWith ()
parseExprTest desc ts ast =
  it desc $ parse (map (Loc ss) $ [ID "f", EQUAL] <> ts <> [SEMICOLON])
  `shouldBe` [ScDef ss "f" [] ast]
